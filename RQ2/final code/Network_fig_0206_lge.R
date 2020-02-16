# Title: Network_fig.R
# Date: 16-Jan-20
# Notes:
# - Modification of networks_1023.R 

# clear workspace
rm(list = ls())
graphics.off()

# Load packages
library(tidyverse)
library(igraph)
library(countrycode)
library(circlize)
library(RColorBrewer)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(cowplot)
library(ggrepel)
library(rgeos)

# Directories
datadir <- "~/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential" #LGE
datadir2 = "~/Documents/SESYNC/Files/FISHMAR-data/rq2/trade_duration" #LGE
datadir3 = "~/Documents/SESYNC/Files/FISHMAR-data/fao_stock_status" #LGE
datadir4 = "~/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/timeseries" #LGE

# Read data
trade <- read.csv(file.path(datadir, "CT_fish_trade92.csv"), as.is=T, stringsAsFactors = FALSE)
stocks  = read.csv(file.path(datadir3, "1950_2017_FAO_bbmsy_timeseries_merge.csv"), as.is=T, stringsAsFactors = FALSE)
match = read.csv(file.path(datadir4,"matchingstocksHS92.csv"), as.is=T, stringsAsFactors = FALSE)
duration <- read.csv(file.path(datadir2, "actual_trade_duration.csv"), as.is=T, stringsAsFactors = FALSE)

# Clean data
match <- match %>%
  select(Shortdescription.HS1992:sci_name) %>%
  filter(!is.na(comm_name))

trade <- trade %>% 
  left_join(match, by = "Shortdescription.HS1992") %>% 
  rename(year=t)

duration <- duration %>%
  select(c('year','group_name','exp_iso3','imp_iso3','actual_duration')) %>% 
  rename(iso3=exp_iso3, imp.iso3=imp_iso3)

# vector with species groups to be removed
remove <- c('salmon','salmonidae','shrimp','tuna') 
stocks  <- stocks %>% 
  left_join(trade, by = c("iso3", "sci_name", "comm_name", "year")) %>% 
  left_join(duration, by = c("iso3", "imp.iso3", "group_name", "year"))%>%
  filter(!is.na(group_name)) %>%
  filter(!(group_name %in% remove)) # remove spp groups

# Create an "all" species group
all <- stocks %>%
  select(iso3, imp.iso3, group_name, year, v, actual_duration, super) %>% 
  distinct() %>% 
  group_by(iso3, imp.iso3, year) %>% 
  summarise(sum_trade = sum(v), max_duration = max(actual_duration), super = mean(super)) # Laura: Does it matter to take the mean of super here?
all$group_name <- "all"

# Calculate species group trade flow totals
stocks <- stocks  %>% 
  select(iso3, imp.iso3, group_name, year, v, actual_duration, super) %>% 
  distinct() %>% 
  group_by(iso3, imp.iso3, year, group_name) %>% 
  summarise(sum_trade = sum(v), max_duration = max(actual_duration), super = mean(super))

# Combine stocks and all dataframes
stocks <- rbind(stocks, all)

# Calculate unweighted average stock status per country per year
stock_status <- stocks %>% 
  group_by(year, iso3, group_name) %>% 
  summarise(mean_super = mean(super)) 

# merge stocks trade and status
network <- stocks %>% 
  left_join(stock_status, by = c("iso3", "group_name", "year")) 

# specify countries to continents
node_order <- data.frame(iso3c = unique(c(network$iso3, network$imp.iso3)))
node_order$region <- countrycode(node_order$iso3, origin = "iso3c", destination = "region") #can change to destination = "region"
node_order$continent <- countrycode(node_order$iso3, origin = "iso3c", destination = "continent")
node_order <- node_order %>%
  group_by(continent) %>%
  arrange(region, .by_group = TRUE)
node_order$iso3c <- as.character(node_order$iso3c)

# assign colors to continents
region_cols <- c(brewer.pal(9, "Greens")[4:8], # "Eastern Africa", "Middle Africa", "Northern Africa", "Southern Africa", "Western Africa" 
  brewer.pal(9, "Purples")[5:8], #"Caribbean", "Central America", "Northern America", "South America"
  brewer.pal(8, "Blues")[3:7], # "Central Asia", "Eastern Asia", "South-Eastern Asia", "Southern Asia", "Western Asia"
  brewer.pal(11, "BrBG")[7:10], # "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe"
  brewer.pal(8, "GnBu")[4:7], # "Australia and New Zealand", "Melanesia", "Micronesia", "Polynesia"
  "grey" #NA
  )
names(region_cols) <- unique(node_order$region)

node_order$region.col <- region_cols[match(node_order$region, names(region_cols))]

# Network plot function
# Create chord diagram
chordT <- function(Group, Year, TopN = 20, title_main = ""){
  # Filter for the species group being plotted
  network <- network %>% 
    filter(group_name == Group)
  
  years <- unique(network$year)
  # Identify biggest trade links to include in the plot
  top.traders <- network %>% 
    ungroup() %>%
    filter(year == years[1]) %>%
    top_n(TopN, sum_trade)
  top.traders <- c(as.character(top.traders$imp.iso3), as.character(top.traders$iso3))
  
  for(i in 2:length(years)){
    top <- network %>% 
      ungroup() %>%
      filter(year == years[1]) %>%
      top_n(TopN, sum_trade)
    
    top.traders<- c(top.traders, as.character(top$imp.iso3), as.character(top$iso3))
  }
  top.traders <- unique(top.traders)
  
  df <- network %>%
    ungroup() %>%
    filter(imp.iso3 %in% top.traders & iso3 %in% top.traders) %>%
    filter(imp.iso3 != "NULL") %>%
    filter(iso3 != "NULL") %>%
    filter(year == Year) %>%
    select(iso3, imp.iso3, max_duration) # change third column to change network weighting
  
  # Format chord diagram
  # Define link colors based on mean_super value
  define.col <- network %>% 
    ungroup() %>%
    filter(imp.iso3 %in% top.traders | iso3 %in% top.traders) %>%
    filter(imp.iso3 != "NULL") %>%
    filter(iso3 != "NULL") %>%
    filter(year == Year) %>%
    mutate(stock_status_col = if_else(mean_super <= 0.5, "orangered4",
                                      if_else(mean_super > 0.5 & mean_super <= 0.75 , "orangered2",
                                              if_else(mean_super > 0.75 & mean_super <= 1, "orange",
                                                      if_else(mean_super > 1, "khaki1", "NA"))))) %>%
    select(iso3, stock_status_col) %>%
    distinct()
  define.col$iso3 <- as.character(define.col$iso3)
  define.col$stock_status_col <- as.character(define.col$stock_status_col)
  
  if(length(top.traders[!(top.traders %in% define.col$iso3)])>0){
    define.col <- define.col %>%
      full_join(data.frame(iso3 = as.character(top.traders[!(top.traders %in% define.col$iso3)]), 
                           stock_status_col = as.character("gray50")), by = c("iso3", "stock_status_col"))
  }else{
    define.col <- define.col
  }
  
  link.col <- define.col$stock_status_col
  names(link.col) <- define.col$iso3
  
  # Define border colors based on region
  define.col$region <- countrycode(define.col$iso3, origin = "iso3c", destination = "region") 
  define.col$region.col <- region_cols[match(define.col$region, names(region_cols))]
  grid.col <- define.col$region.col
  names(grid.col) <- define.col$iso3

  chordDiagram(df, grid.col = grid.col, col = link.col,
               directional = 1, direction.type = c("diffHeight", "arrows"), link.arr.type = "big.arrow", 
               order = node_order$iso3c, annotationTrack = c("", "grid", ""))
  
  title(paste(paste(title_main), "\n", paste(Year)))
}

# choose species, year and top trade flows to plot
chordT(Group = "plaice", Year = 2015, TopN = 25)

#network plots
groups <- unique(network$group_name)
for(i in 1:length(groups)){ 
  png(filename = file.path("Output", "chord_diagrams", paste(groups[i],".png", sep="")), width = 600, height = 200)
  layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))  
  par(mai = c(.1, .1, .4, .1)) # bottom, left, top, right
  chordT(Group = groups[i], Year = years[1], TopN = 25)
  chordT(Group = groups[i], Year = years[2], TopN = 25, title_main = groups[i])
  chordT(Group = groups[i], Year = years[3], TopN = 25)
  dev.off()
}

# Plot world map with region colors
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- full_join(world, node_order, by = c("su_a3" = "iso3c"))
g <- ggplot(data = world) +
  geom_sf(fill = world$region.col) +
  theme_minimal() 

png(filename = file.path("Output", "chord_diagrams", "region_map.png", sep=""), width = 600, height = 400)
g
dev.off()


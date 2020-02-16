# Title: SI_plots.R
# Date: 16-Feb-20
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

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### PLOT FIGURE SI_FigureS4 ###
r1 <- ggplot(network, aes(x=sum_trade, y=max_duration)) + 
  geom_point() + 
  labs(y='Maximum trade duration [years]', x='Traded volume [kg]') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
###  mobility plots ### 
mlow = c('crab','homarus','lobster','mussel','oyster','rocklobster','scallop') 
mmed = c('coalfish','cuttlefish','flatfish','haddock','hake','halibut','octopus','plaice','seabass','sole') 
mhigh = c('cod','eel','shark')  

network <- na.omit(network)   
temp <- aggregate(network[, 6:8], list(network$group_name), mean)

mobl = subset(temp,(Group.1 %in% mlow)) # include groups
mobm = subset(temp,(Group.1 %in% mmed)) # include groups
mobh = subset(temp,(Group.1 %in% mhigh)) # include groups

r2 <- ggplot(mobl, aes(x = max_duration, y = mean_super, color=Group.1, group=Group.1)) + geom_point()
r3 <- ggplot(mobm, aes(x = max_duration, y = mean_super, color=Group.1, group=Group.1)) + geom_point()
r4 <- ggplot(mobh, aes(x = max_duration, y = mean_super, color=Group.1, group=Group.1)) + geom_point()

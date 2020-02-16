# this file calculates the number of stocks in the dataset, it builds on the script
# clear workspace
rm(list = ls())
graphics.off()

# Packages
library(tidyverse)
library(igraph)
library(ineq)
library(sjstats)
library(glmmTMB)
library(data.table)
library(base)
require(dplyr)

# Directories
datadir <- "~/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential" #LGE
datadir2 = "~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data" #LGE
datadir3 = "~/Documents/SESYNC/Files/FISHMAR-data/fao_stock_status" #LGE
datadir4 = "~/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/timeseries" #LGE

# Read data
data_orig <- read.csv(file.path(datadir, "CT_fish_trade92.csv"), as.is=T)
stocks  = read.csv(file.path(datadir3, "1950_2017_FAO_bbmsy_timeseries_merge.csv"), as.is=T)
match = read.csv(file.path(datadir4,"matchingstocksHS92.csv"), as.is=T)
trade_duration <- read.csv(file.path(datadir2, "trade_duration.csv"), as.is=T)

######################################  # original stocks in data  ##########################################
# Are there only unique super per species (sci_name), year, country?
test_uniq <- data.frame(species = stocks$sci_name, year = stocks$year, stockid =stocks$stockid)
stocks_uniq <- subset(stocks, (duplicated(test_uniq)) & !(is.na(super)))
#there are no duplicates in stocks_uniq

# what is the number of stocks in the original data? 
test_uniq$isospp <- paste(test_uniq$species, test_uniq$stockid, sep="_")
stocks_uniqnum <- test_uniq %>% 
  distinct(test_uniq$isospp, .keep_all = T)
#1740 stocks in stocks_uniqnum!

######################################  Format data  ##########################################
# Format data
data <- data_orig %>% 
  rename(year=t, hs92desc=Shortdescription.HS1992, hs92code=Code.HS1992,
         exp_iso3=iso3, imp_iso3=imp.iso3, value_usd=v, quantity_mt=q, group=group_name) %>% 
  select(imp_iso3, group, hs92code, hs92desc, exp_iso3, year, value_usd, quantity_mt) %>% 
  arrange(imp_iso3, group, hs92code, hs92desc, exp_iso3, year)

match = match %>% select(Shortdescription.HS1992, comm_name, sci_name)%>% 
  rename(hs92desc=Shortdescription.HS1992) 

match = data %>% left_join(match)

stocks = stocks %>% select(sci_name, comm_name, super, year, iso3) %>% 
  rename(exp_iso3 = iso3)%>% left_join(match) %>% 
  filter(!is.na(super))%>%filter(!is.na(hs92desc)) %>%
  select(exp_iso3, sci_name, group, year, super)


### remove sci_name multiple matches to group
# new df
observation <- data.frame(sci_name = match$sci_name, group = match$group) # new df including sciname,group
# remove all spp we will rm later
remove = c('salmon','salmonidae','shrimp','tuna') # vector with groups to be removed
t1 = subset(observation,!(group %in% remove)) # remove groups
# unique pairs
t2 <- unique(t1[c("sci_name","group")]) # new df incl only unique values of sci_name group pairs
# check 
t3 <- t2[1] # used in next line
t4 <- subset(t2, (duplicated(t3))) # new df incl only duplicate observations of sci_name
# now choose all duplicates and remove them from the match data after line 40 
t5 <- unique(t4[c("sci_name")]) # 
t6 = t2[(t2$sci_name %in% t5$sci_name),] # identify sci_name with double matches to group
# identified wrong matches
fal <- data.frame("sci_name" = c("Homarus americanus","Homarus gammarus","Jasus edwardsii","Jasus lalandii","Jasus novaehollandiae","Jasus paulensis","Jasus paulensis","Jasus tristani","Jasus tristani","Metanephrops challengeri","Palinurus elephas","Palinurus gilchristi","Panulirus argus","Panulirus argus","Panulirus cygnus","Panulirus homarus","Paralichthys olivaceus","Reinhardtius hippoglossoides","Thenus orientalis"),"group" = c("lobster","lobster","lobster","lobster","homarus","homarus","flatfish","homarus","flatfish","homarus","homarus","homarus","lobster","flatfish","lobster","homarus","flatfish","flatfish","homarus"))

# new columns to remove the wrong matches
match$grsci <- paste(match$group, match$sci_name, sep="_") 
fal$grsci <- paste(fal$group, fal$sci_name, sep="_")

match = match[!(match$grsci %in% fal$grsci),] # identify sci_name with double matches to group


######################################  Calc # stocks remaining  ##########################################
#### remove aquaculture and environmental variability driven species
# check which groups are there
check = match %>% select('group') 
check= unique(check)

# spp to remove
remove = c('salmon','salmonidae','shrimp','tuna') #vector with groups to be removed
ndata = subset(match,!(group %in% remove))

# create a matching file btw group and sci_name 
nmatch <- ndata[,c("group","sci_name")]
nmuniq <- nmatch %>% 
  distinct(group, sci_name, .keep_all = F)

# rename, leftjoin to remove groups not included and spp without matches
nmuniq <- nmuniq %>% 
  rename(species=sci_name)
nspecies = stocks_uniqnum %>% left_join(nmuniq)  %>% 
  filter(!is.na(group))
nsppfinal = subset(nspecies,!(group %in% remove))
#746 stocks in stocks_uniqnum!









######################################  Calc # stocks calculation  ##########################################
# remove from original data
volatility_duration_clean = subset(volatility_duration,!(group %in% remove))

# leftjoin data and stocks
volatility_duration_stocks = volatility_duration_clean %>% left_join(stocks) %>%
  distinct()

# leftjoin data and stocks
volatility_duration_stocks_allspp = volatility_duration %>% left_join(stocks) %>%
  distinct()

observation <- data.frame(exp = volatility_duration_stocks$exp_iso3, group = volatility_duration_stocks$group, year = volatility_duration_stocks$year)
x <- unique(volatility_duration_stocks[c("exp_iso3", "group","year")])
y <- unique(x[c("exp_iso3", "group")])
ltd_stocks <- subset(volatility_duration_stocks, (duplicated(observation)))

observation2 <- data.frame(exp = volatility_duration_stocks_allspp$exp_iso3, group = volatility_duration_stocks_allspp$group, year = volatility_duration_stocks_allspp$year)
all_stocks <- subset(volatility_duration_stocks_allspp, (duplicated(observation2)) & !(is.na(super)))

# this file is intended to provide timeseries of co-variates 
# clear workspace
rm(list = ls())
graphics.off()

### libraries
require(dplyr)
library(tidyr)
library(data.table)

### directories
datadir <- "~/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential"
datadir2 = "~/Documents/SESYNC/Files/FISHMAR-data/rq2/trade_duration_AG"
datadir3 = "~/Documents/SESYNC/Files/FISHMAR-data/fao_stock_status"
datadir4 = "~/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/timeseries"
datadir5 = "~/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/governance"
datadir6 = "~/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/gravdata_cepii"

### read data
tra <- read.csv(file.path(datadir, "CT_fish_trade92.csv"), as.is=T) # trade data 
stocks  = read.csv(file.path(datadir3, "1950_2017_FAO_bbmsy_timeseries_merge.csv"), as.is=T) # stocks data
mat1 = read.csv(file.path(datadir4,"matchingstocksHS92.csv"), as.is=T) #  trade and stocks name
mat2 = read.csv(file.path(datadir,"trade_groups_for_new_links92.csv"), as.is=T) # trade and group name
collapse <- read.csv(file.path(datadir, "trade_collapse.csv"), as.is=T) # trade collapse
duration <- read.csv(file.path(datadir2, "exports_test.csv"), as.is=T) # trade duration
gini = read.csv(file.path(datadir, "gini_and_clustering.csv"), as.is=T) # gini and clustering
gov = read.csv(file.path(datadir5, "governance_effective_processed.csv"), as.is=T) # governance
loc = read.csv(file.path(datadir6, "dist_cepii.csv"), as.is=T) # geographic distance
# add link turnover, top 3 importers % traded volume
# consider GPS data, profitability

### prepare data
# rename column in files # show files with head(mat1)
colnames(mat1)[colnames(mat1)=="Shortdescription.HS1992"] <- "HS92" # an alternative way of reanming is # mat1 = mat1 %>% rename(HS92 = Shortdescription.HS1992)
colnames(mat2)[colnames(mat2)=="X...Shortdescription.HS1992"] <- "HS92" 
colnames(tra)[colnames(tra)=="t"] <- "year" 
# join files by columns
mat1= left_join(x=mat1, y = mat2, by= "HS92",  all.x=TRUE) # mat1 mat2
# clean mat1: only two columns, delete duplicates, omit na´s
mat3 = mat1 %>% distinct() %>% select(sci_name, group_name)
mat3 = unique(mat3)
mat3 = na.omit(mat3)
# add mat3 to stocks
stocks = left_join(x=stocks, y = mat3) 

### calculate average BBmsy
mean_super <- aggregate(super ~ group_name + iso3 + year, stocks, mean) # calculate mean per group, country, year
tra = left_join(x=tra, y = mean_super) # match trade and mean_super
tra = na.omit(tra) # omit na´s
trade_mean_super <- aggregate(super ~ group_name + year, tra, mean) # mean BBmsy of export countries per year and spp group
colnames(trade_mean_super)[colnames(trade_mean_super)=="super"] <- "per_trade" # change column name
# calculate weighted average i.e. super weighted by traded volume
tra$weigh <- tra$v *tra$super
weighted_mean_super <- aggregate(weigh ~ group_name + year, tra, mean)
weighted_mean_super$v <- aggregate(v ~ group_name + year, tra, mean)
weighted_mean_super$weighted <- weighted_mean_super$weigh/weighted_mean_super$v$v 
# add average bbmsy by trade and weighted by volume into bbmsy
bbmsy = weighted_mean_super %>% select(group_name, year, weighted) %>% 
  left_join(trade_mean_super) 



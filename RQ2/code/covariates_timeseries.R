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
gini$X <- NULL # remove column X from gov
names(gini)[1]<-"group_name" # change column name

### calculate average BBmsy
# match trade and stock data
mat1= left_join(x=mat1, y = mat2, by= "HS92",  all.x=TRUE) # join files by columns
# clean mat1: only two columns, delete duplicates, omit na´s
mat3 = mat1 %>% distinct() %>% select(sci_name, group_name)
mat3 = unique(mat3)
mat3 = na.omit(mat3)
stocks = left_join(x=stocks, y = mat3) # add mat3 to stocks
mean_super <- aggregate(super ~ group_name + iso3 + year, stocks, mean) # calculate mean per group, country, year
tra_b = left_join(x=tra, y = mean_super) # match trade and mean_super
tra_b = na.omit(tra_b) # omit na´s
# calculate mean bbmsy
trade_mean_super <- aggregate(super ~ group_name + year, tra_b, mean) # mean BBmsy of export countries per year and spp group
colnames(trade_mean_super)[colnames(trade_mean_super)=="super"] <- "tradelink_super" # change column name
# calculate weighted average i.e. super weighted by traded volume
tra_b$weigh <- tra_b$v *tra_b$super
weighted_mean_super <- aggregate(weigh ~ group_name + year, tra_b, sum)
weighted_mean_super$v <- aggregate(v ~ group_name + year, tra_b, sum)
weighted_mean_super$weighted_super <- weighted_mean_super$weigh/weighted_mean_super$v$v 
# add average bbmsy by trade and weighted by volume into bbmsy
bbmsy = weighted_mean_super %>% select(group_name, year, weighted_super) %>% 
  left_join(trade_mean_super) 

### trade collapses: number and percent
names(collapse)[2]<-"year" # change column name
coll <- aggregate(collapse ~ group_name + year, collapse, sum) # sum collapses per spp group per year
names(coll)[3]<-"number_collapse" # change column name
coll2 <- collapse %>% group_by(group_name, year) %>% summarise(n()) # count occurrences of trades in a spp group per year i.e. conditional occurrences
names(coll2)[3]<-"number_trade" # change column name
trade_collapse = left_join(x=coll, y = coll2) # match coll and coll2
trade_collapse$percent_collapse <- trade_collapse$number_collapse/trade_collapse$number_trade # calculate percentage collapsed
trade_collapse$number_trade <- NULL # remove column 

### trade duration
names(duration)[2]<-"year" # change column name
trade_duration <- aggregate(duration ~ group_name + year, duration, mean) # sum collapses per spp group per year
names(trade_duration)[3]<-"avg_duration" # change column name

### governance effectiveness: average effectiveness of exporters
names(gov)[2]<-"year" # change column name
gov$X <- NULL # remove column X from gov
tra_gov = left_join(x=tra, y = gov) # match trade and governance
tra_gov = na.omit(tra_gov) # omit na´s
tra_gov2 <- aggregate(gov_effectiveness ~ group_name + year, tra_gov, mean) # mean governance of trade links per spp group per year
names(tra_gov2)[3]<-"tradelink_gov" # change column name
# calculate weighted average i.e. governance weighted by traded volume
tra_gov$weigh <- tra_gov$v *tra_gov$gov_effectiveness
weighted_gov <- aggregate(weigh ~ group_name + year, tra_gov, sum)
weighted_gov$v <- aggregate(v ~ group_name + year, tra, sum)
weighted_gov$weighted_gov <- weighted_gov$weigh/weighted_gov$v$v 
# add average governance by trade and weighted by volume into governance
governance = weighted_gov %>% select(group_name, year, weighted_gov) %>% 
  left_join(tra_gov2) 

### create one dataframe
# join all timeseries
data <- left_join(bbmsy, gini) %>%
  left_join(., trade_collapse) %>%
  left_join(., trade_duration) %>%
  left_join(., governance) 
# write csv file
write.csv(data, file.path(datadir, "covariates_timeseries.csv"))




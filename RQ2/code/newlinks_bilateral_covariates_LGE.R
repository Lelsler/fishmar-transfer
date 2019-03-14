# in this file I add the co-variates to the newlinks file, we can use only unilateral co-variates as the file does not contain tarding partners
#clear workspace
rm(list = ls())
graphics.off()

setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2")

require(dplyr)
library(tidyr)
library(data.table)

### load required data
# bilateral trade data
tra = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/newnewlinks.bilateral.csv") 
# species group matching and stock status
mat1 = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/timeseries/matchingstocksHS92.csv") # HS92 (from trade) matched to comm_name, sci_name (stock status file)
mat2 = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/trade_groups_for_new_links92.csv") 
sto = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/fao_stock_status/1950_2017_FAO_bbmsy_timeseries_merge.csv") 

# rename column in files # show files with head(mat1)
colnames(mat1)[colnames(mat1)=="Shortdescription.HS1992"] <- "HS92" # an alternative way of reanming is # mat1 = mat1 %>% rename(HS92 = Shortdescription.HS1992)
colnames(mat2)[colnames(mat2)=="X...Shortdescription.HS1992"] <- "HS92" 
colnames(mat2)[colnames(mat2)=="group_name"] <- "our_group_name" 

# join files by columns
mat1= left_join(x=mat1, y = mat2, by= "HS92",  all.x=TRUE) # mat1 mat2

# clean mat1: only two columns, delete duplicates, omit na´s
mat3 = mat1 %>% distinct() %>% select(sci_name, our_group_name)
mat3 = unique(mat3)
mat3 = na.omit(mat3)

### match 
# match tra and mat3
tra1 = left_join(x=tra, y = mat3) 
# change column name of iso exporter in stock
colnames(sto)[colnames(sto)=="iso3"] <- "exp_iso3" 
# clean sto
sto1 = sto %>% distinct() %>% select(sci_name, super, exp_iso3, year) 
# match tra1 and stock status super
tra4 = left_join(x=tra1, y = sto1) 
# omit na´s
tra4 = na.omit(tra4)

###! add co-variates
# country co-variates
gps = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/global_preference/GPS_dataset_country_level/country.csv") # global preference survey indicators
iso = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/timeseries/country_code_baci92_2.csv") # iso country codes for i,j 
loc = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/gravdata_cepii/dist_cepii.csv") # geographic distance (and other gravity data)
gov = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/governance/governance_effective_processed.csv") # global preference survey indicators

# join the global preference factors for importers and exporters
colnames(gps)[colnames(gps)=="iso3"] <- "exp_iso3" 
tco =left_join(x=tra4, y =gps[ , c("exp_iso3","posrecip","negrecip","trust")], by = "exp_iso3",  all.x=TRUE) 
setnames(tco, old=c("posrecip","negrecip","trust"), new=c("exp_posrecip", "exp_negrecip","exp_trust"))
colnames(gps)[colnames(gps)=="exp_iso3"] <- "imp_iso3" 
tco =left_join(x=tco, y =gps[ , c("imp_iso3","posrecip","negrecip","trust")], by = "imp_iso3",  all.x=TRUE) 
setnames(tco, old=c("posrecip","negrecip","trust"), new=c("imp_posrecip", "imp_negrecip","imp_trust"))

# join cepii distance, colonial history, common language
setnames(loc, old=c("iso_o","iso_d"), new=c("imp_iso3", "exp_iso3"))
tco = left_join(x=tco, y = loc[ , c("imp_iso3","exp_iso3","comlang_ethno","colony","distcap")], all.x=TRUE) 

## join governance effectiveness
# new and change name columns
gov$exp_iso3 <- gov$iso3
colnames(gov)[colnames(gov)=="t"] <- "year" 
# add gov for exporter
tco = left_join(x=tco, y = gov[ , c("exp_iso3","year","gov_effectiveness")], all.x=TRUE) 
colnames(tco)[colnames(tco)=="gov_effectiveness"] <- "exp_gov_eff" 
# add gov for importer
colnames(gov)[colnames(gov)=="exp_iso3"] <- "imp_iso3" 
tco = left_join(x=tco, y = gov[ , c("imp_iso3","year","gov_effectiveness")], all.x=TRUE) 
colnames(tco)[colnames(tco)=="gov_effectiveness"] <- "imp_gov_eff" 

##! change column old and new trade connections to numbers for calculations  
ta = tco %>%
  mutate(new.export=case_when(
    new.export=="old_connection" ~ 0,
    new.export=="new_connection" ~ 1
  ))

tat = ta %>%
  mutate(new.import=case_when(
    new.import=="old_connection" ~ 0,
    new.import=="new_connection" ~ 1
  ))

# save csv file 
write.csv(tat, "/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/bilateral_newlinks_covariates.csv")

# load files
tdc = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/bilateral_newlinks_covariates.csv") 
#tdc = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/bilateral_newlinks_covariates.csv") 



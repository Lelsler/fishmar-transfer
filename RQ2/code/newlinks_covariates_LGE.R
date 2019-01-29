# in this file I add the co-variates to the newlinks file, we can use only unilateral co-variates as the file does not contain tarding partners
#clear workspace
rm(list = ls())
graphics.off()

setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2")

require(dplyr)
library(tidyr)

#load required data
tra = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/newlinks_covariates2.csv") 
gps = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/global_preference/GPS_dataset_country_level/country.csv") # global preference survey indicators
iso = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/timeseries/country_code_baci92_2.csv") # iso country codes for i,j 
loc = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/gravdata_cepii/dist_cepii.csv") # geographic distance (and other gravity data)

colnames(tra)[colnames(tra)=="country"] <- "iso3"

# join the global preference factors
covariates= left_join(x=tra, y = gps[ , c("iso3","patience","risktaking","posrecip","negrecip","altruism","trust")], by = "iso3",  all.x=TRUE) 

# save csv file 
write.csv(covariates, "/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/newlinks_covariates_LGE.csv")


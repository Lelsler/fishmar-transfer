#clear workspace
rm(list = ls())
graphics.off()

setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2")

require(dplyr)
library(tidyr)

#load required data
cepi = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/timeseries/comtrade_1992_year1995_with_stocks.csv") # stocks and comtrade matches
gps = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/global_preference/GPS_dataset_country_level/country.csv") # global preference survey indicators
iso = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/timeseries/country_code_baci92_2.csv") # iso country codes for i,j 
loc = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/gravdata_cepii/dist_cepii.csv") # geographic distance (and other gravity data)
goveff = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/governance/governance_effective_processed.csv") # governance effectiveness indicators

### match geographic distance to covariates
# match importers to iso codes
covariates= left_join(cepi, iso, by = "j", copy = TRUE, all = TRUE) 

# create a new colum where we match iso of importer and exporter
covariates$exp_imp <- do.call(paste, c(covariates[c("iso3", "iso3_imp")], sep = ""))
loc$exp_imp <- do.call(paste, c(loc[c("iso_o", "iso_d")], sep = ""))

# match and select columns to be merged (distcap = distance capitals)
covariates = left_join(x = covariates, y = loc[ , c("exp_imp","distcap")], by = "exp_imp", all.x=TRUE)

### match global preferences (gps) to covariates
# covariates= left_join(covariates, gps, by = "iso3", copy = TRUE, all = TRUE) 
covariates= left_join(x=covariates, y = gps[ , c("iso3","patience","risktaking","posrecip","negrecip","altruism","trust")], by = "iso3",  all.x=TRUE) 

### matching of governance effectiveness to covariates
# delete X in front of the column names
colnames(goveff) <- gsub("^X", "",  colnames(goveff))

# point R to column names in df
colnames(goveff) = c("country", "iso3", "1996", "1997", 1998:2016)

# re-format gov
gov = goveff %>%
  gather(t, gov_eff, -country, -iso3)

# match gov to covariates ## Error in left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y, check_na_matches(na_matches)) : 
# Can't join on 't' x 't' because of incompatible types (character / integer)
# covariates= left_join(covariates, gov, by=c("iso3","t"), copy = TRUE, all = TRUE) 
# covariates= left_join(x=covariates, y=gov, by=c("iso3","t"), all.x=TRUE) 


# save data file comtrade_1992_year1995_with_stock and gps co-variate
write.csv(covariates, "/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/comtrade_stock_covariates_92_16.csv")










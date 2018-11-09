#clear workspace
rm(list = ls())
graphics.off()

setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2")

require(dplyr)

#load required data
cepi = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/comtrade_1992_year1995_with_stocks.csv") # stocks and comtrade matches
gps = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/global_preference/GPS_dataset_country_level/country.csv") # global preference survey indicators
iso = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/timeseries/country_code_baci92_2.csv") # iso country codes for i,j 
loc = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/gravdata_cepii/dist_cepii.csv") # geographic distance (and other gravity data)

# match exporters to iso codes
covariates= left_join(cepi, iso, by = "j", copy = TRUE, all = TRUE) 

# create a new colum where we match iso of importer and exporter
covariates$exp_imp <- do.call(paste, c(covariates[c("iso3", "iso3_imp")], sep = ""))
loc$exp_imp <- do.call(paste, c(loc[c("iso_o", "iso_d")], sep = ""))

# match and select columns to be merged (distcap = distance capitals)
covariates = merge(x = covariates, y = loc[ , c("exp_imp","distcap")], by = "exp_imp", all.x=TRUE)

#match gps by country codes
#covariates = merge(x = covariates, y = gps, by = "iso3", all = TRUE)
#covariate = gps%>% left_join(cepi,  by = "iso3")
covariates= left_join(covariates, gps, by = "iso3", copy = TRUE, all = TRUE) 


# save data file comtrade_1992_year1995_with_stock and gps co-variate
write.csv(covariates, "/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/comtrade_stock_covariates_92_16.csv")










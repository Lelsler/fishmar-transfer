#clear workspace
rm(list = ls())
graphics.off()

setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data")

require(dplyr)

#load required data
cepi = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/comtrade_1992_year1995_with_stocks.csv")
gps = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/global_preference/GPS_dataset_country_level/country.csv")

#match gps by country codes
covariates = merge(x = cepi, y = gps, by = "iso3", all = TRUE)
#covariate = gps%>% left_join(cepi,  by = "iso3")
covariates= left_join(cepi, gps, by = "iso3", copy = TRUE, all = TRUE) 

# save data file comtrade_1992_year1995_with_stock and gps co-variate
write.csv(covariates, "/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/comtrade_stock_gps_92_16.csv")

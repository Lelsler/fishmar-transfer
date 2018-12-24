#clear workspace
rm(list = ls())
graphics.off()

setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2")

require(dplyr)
library(tidyr)
# Loading
library("readxl")
# xls files

#load required data
cen <- read_excel("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/maps_data/country_centroids.xlsx", sheet = "Sheet1") # country spatial centroids
iso <- read_excel("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/maps_data/iso2_iso3_conversion.xlsx", sheet = "Sheet1") # iso codes

# match importers to iso codes
cce = left_join(x=cen, y=iso[ , c("iso2","iso3")], by = "iso2", copy = TRUE, all = TRUE) 

# save data file comtrade_1992_year1995_with_stock and gps co-variate
write.csv(cce, "/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/maps_data/country_centroids.csv")

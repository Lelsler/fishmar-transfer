#clear workspace
rm(list = ls())
graphics.off()

setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2")

require(dplyr)
library(tidyr)

#load required data
cepi = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/timeseries/comtrade_1992_year1998_with_stocks.csv") # stocks and comtrade matches
gps = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/global_preference/GPS_dataset_country_level/country.csv") # global preference survey indicators
iso = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/timeseries/country_code_baci92_2.csv") # iso country codes for i,j 
loc = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/gravdata_cepii/dist_cepii.csv") # geographic distance (and other gravity data)
gov_d = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/governance/governance_effective.csv") # governance effectiveness indicators

### match geographic distance to covariates
# match importers to iso codes
covariates= left_join(cepi, iso, by = "j", copy = TRUE, all = TRUE) 

# create a new colum where we match iso of importer and exporter
covariates$exp_imp <- do.call(paste, c(covariates[c("iso3", "iso3_imp")], sep = ""))
loc$exp_imp <- do.call(paste, c(loc[c("iso_o", "iso_d")], sep = ""))

# match and select columns to be merged (distcap = distance capitals)
covariates = left_join(x = covariates, y = loc[ , c("exp_imp","distcap")], by = "exp_imp", all.x=TRUE)

### match global preferences (gps) to covariates
covariates= left_join(x=covariates, y = gps[ , c("iso3","patience","risktaking","posrecip","negrecip","altruism","trust")], by = "iso3",  all.x=TRUE) 

### matching of governance effectiveness to covariates
# delete X in front of the column names
colnames(gov_d) <- gsub("^X", "",  colnames(gov_d))

# indicate column names to R
colnames(gov_d) = c("country", "iso3", "1996", "1998", "2000", 2002:2016)

# re-arrange dataframe to long format
gov_eff = gov_d  %>% gather(t, gov_effectiveness, -iso3, -country)

## add missing years
# 1995 = 1996
mean_gov_eff_1995 = data.frame(gov_eff$gov_effectiveness[gov_eff$t==1996], 1995, gov_eff$iso3[gov_eff$t==1996], gov_eff$country[gov_eff$t==1996])
colnames(mean_gov_eff_1995) = c("gov_effectiveness", "t", "iso3", "country")

#mean for 1997
mean_gov_eff_1997 = data.frame(gov_eff$gov_effectiveness[gov_eff$t==1996]+gov_eff$gov_effectiveness[gov_eff$t==1998]/2, 1997, gov_eff$iso3[gov_eff$t==1998], gov_eff$country[gov_eff$t==1998])
colnames(mean_gov_eff_1997) = c("gov_effectiveness", "t", "iso3", "country")

#mean for 1999
mean_gov_eff_1999 = data.frame(gov_eff$gov_effectiveness[gov_eff$t==1998]+gov_eff$gov_effectiveness[gov_eff$t==2000]/2, 1999, gov_eff$iso3[gov_eff$t==2000], gov_eff$country[gov_eff$t==2000])
colnames(mean_gov_eff_1999) = c("gov_effectiveness", "t", "iso3", "country")

#mean for 2001
mean_gov_eff_2001 = data.frame(gov_eff$gov_effectiveness[gov_eff$t==2000]+gov_eff$gov_effectiveness[gov_eff$t==2002]/2, 2001, gov_eff$iso3[gov_eff$t==2000], gov_eff$country[gov_eff$t==2000])
colnames(mean_gov_eff_2001) = c("gov_effectiveness", "t", "iso3", "country")

# inlude missing years into gov_eff
gov = rbind(mean_gov_eff_1995, mean_gov_eff_1997, mean_gov_eff_1999, mean_gov_eff_2001, gov_eff)

# convert t (years) into integers for merging
gov$t <- as.integer(gov$t)

# match gov to covariates 
# covariates= left_join(covariates, gov, by=c("iso3","t"), copy = TRUE, all = TRUE) 
covariates= left_join(x=covariates, y=gov[, c("iso3","t","gov_effectiveness")], by=c("iso3","t"), all.x=TRUE) 

# save data file comtrade_1992_year1995_with_stock and gps co-variate
write.csv(covariates, "/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/ts_covariates/comtrade_stock_covariates_year98.csv")










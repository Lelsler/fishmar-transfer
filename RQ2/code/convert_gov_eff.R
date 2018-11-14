#clear workspace
rm(list = ls())
graphics.off()

setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2")

require(dplyr)
library(tidyr)

#load required data
gov_d = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/governance/governance_effective.csv") # governance effectiveness indicators

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

#load required data
#goveff = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/governance/governance_effective_processed.csv") # governance effectiveness indicators

# delete X in front of the column names
#colnames(goveff) <- gsub("^X", "",  colnames(goveff))

# point R to column names in df
#colnames(goveff) = c("country", "iso3", "1996", "1997", 1998:2016)

# re-format gov
#gov = goveff %>%
#  gather(t, gov_eff, -country, -iso3)


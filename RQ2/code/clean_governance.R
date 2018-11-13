#clear workspace
rm(list = ls())
graphics.off()

setwd("/Users/mtn1/Nextcloud/FISHMAR-data/rq2/co_variates_data/governance/")

require(dplyr)
library(tidyr)

#load required data
gov_d = read.csv("governance_effective.csv") # governance effectiveness indicators

colnames(gov_d) = c("country", "iso3", "1996", "1998", "2000", 2002:2016)

gov_eff = gov_d  %>% gather(year, effectiveness, -iso3, -country)

#mean for 1997
mean_gov_eff_1997 = data.frame(gov_eff$effectiveness[gov_eff$year==1996]+gov_eff$effectiveness[gov_eff$year==1998]/2, 1997, gov_eff$iso3)
colnames(mean_gov_eff_1997) = c("effectiveness", "year", "iso3")

#mean for 1999
mean_gov_eff_1999 = data.frame(gov_eff$effectiveness[gov_eff$year==1998]+gov_eff$effectiveness[gov_eff$year==2000]/2, 1999, gov_eff$iso3)
colnames(mean_gov_eff_1999) = c("effectiveness", "year", "iso3")

#mean for 2001
mean_gov_eff_2001 = data.frame(gov_eff$effectiveness[gov_eff$year==2000]+gov_eff$effectiveness[gov_eff$year==2002]/2, 2001, gov_eff$iso3)
colnames(mean_gov_eff_2001) = c("effectiveness", "year", "iso3")

gov_eff = rbind(mean_gov_eff_1997, mean_gov_eff_1999, mean_gov_eff_2001, gov_eff)
# Clear workspace
rm(list = ls())
graphics.off() 

# for now this code is only looking at net increases and decreases in node-degree, while we are actually interested 
# in all the new connections that are formed. 

# libraries
require(igraph)
require(dplyr)
require(reshape2)

#setwd("/Users/mtn1/Dropbox/SFI/Before_November/ORIGINAL_DATA_FILES")
setwd("/Users/mtn1/Nextcloud/FISHMAR-data/rq2/test_preferential/")


#all new links
nl = read.csv("new_links.csv") #this works only if commas in orignial file are removed

# choose year & species
Rawdata.1995 <- read.csv("/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/processed/ts_covariates/comtrade_stock_covariates_year95.csv")
Rawdata.1996 <- read.csv("/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/processed/ts_covariates/comtrade_stock_covariates_year96.csv")
Rawdata.1997 <- read.csv("/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/processed/ts_covariates/comtrade_stock_covariates_year97.csv")
Rawdata.1998 <- read.csv("/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/processed/ts_covariates/comtrade_stock_covariates_year98.csv")

data1 = bind_rows(Rawdata.1995, Rawdata.1996, Rawdata.1997, Rawdata.1998) #gives warnings that don't matter

data1 = data1 %>% select(t, iso3,  stockid, fao_area, super, Commodity, gov_effectiveness) %>%
  rename(year=t, commodity = Commodity, country=iso3)
# I took the "super" from the stock estimates, not entirely sure on that one

data = data1 %>% left_join(nl, by = c("year", "country", "commodity"))

write.csv(data, "newlinks_covariates.csv")
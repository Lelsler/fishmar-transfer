# creates a file inlcuding 
#clear workspace
rm(list = ls())
graphics.off()
require(dplyr)

# set working directory
#setwd("/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/processed/timeseries")
setwd("/Users/Ping/Desktop/data/FISHMAR-data/comtrade/processed/timeseries") #LGE

### read data
# downloaded matching files
hs_to_hs = read.csv("1992_to_2012_flatfish.csv") # HS1992 and HS2012
fao_trade_to_stocks = read.csv("trade_to_stocks.csv") # FAO Stock_ID and ID_FAO trade
ct_to_fao = read.csv("ct_to_fao.csv") # ID_FAO trade and HS2012
# self produced matching files
faostocks = read.csv("stocks_match.csv") # FAO Stock_ID, species, iso3, years, id_fao_trade, id_comtrade
# data files
stocks = read.csv("/Users/Ping/Desktop/data/FISHMAR-data/fao_stock_status/1950_2017_FAO_bbmsy_timeseries_merge.csv") # stockid, iso3, year, comm_name, super  
trade = read.csv("CT_fish_trade92.csv")

# cleand data: rename columns, select columns
names(ct_to_fao)[1]<-"ID_FAOTrade" # change column name
names(fao_trade_to_stocks)[1]<-"main_id_relation" # change column name
names(hs_to_hs)[1]<-"Commodity.HS2012code" # change column name
names(faostocks)[1]<-"Stock_Id" # change column name
names(faostocks)[8]<-"sci_name" # change column name
faostocks <- faostocks %>% select("Stock_Id", "fao_code", "fao_area", "iso3", "country", "comm_name","sci_name")
stocks <- stocks %>% select("fao_code", "fao_area", "iso3", "country", "comm_name","sci_name","iso3", "year", "super")

### merging data
CT92_to_fao_trade = ct_to_fao %>% left_join(hs_to_hs, by = c("Commodity.HS2012code", "Commodity.HS2012"))
CT92_to_stock_id = CT92_to_fao_trade %>% left_join(fao_trade_to_stocks, by = c("ID_FAOTrade", "Commodity"))
CT92_to_stock_match = CT92_to_stock_id %>% left_join(faostocks, by = "Stock_Id")
CT92_to_actual_stocks = CT92_to_stock_match %>% left_join(stocks, by = c("fao_code", "fao_area", "iso3", "country", "comm_name","sci_name"))

# select and save
min_match = CT92_to_actual_stocks %>% select(Shortdescription.HS1992, comm_name, sci_name)%>%distinct()
write.csv(min_match, "New_matchingstocksHS92.csv")






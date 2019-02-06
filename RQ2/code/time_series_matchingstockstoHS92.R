#clear workspace
rm(list = ls())
graphics.off()
require(dplyr)

setwd("/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/processed/timeseries")

codes = read.csv("timeseries.needed.csv")
hs_to_hs = read.csv("1992_to_2012.csv")
ct_to_fao = read.csv("ct_to_fao.csv")
fao_trade_to_stocks = read.csv("trade_to_stocks.csv")
stocks = read.csv("stocks_match.csv")
stock = read.csv("/Users/mtn1/Nextcloud/FISHMAR-data/fao_stock_status/1950_2017_FAO_bbmsy_timeseries_merge.csv")
country_codes = read.csv("country_code_baci92.csv")


CT92_to_fao_trade = ct_to_fao %>% left_join(hs_to_hs, by = c("Commodity.HS2012code", "Commodity.HS2012"))
CT92_to_stock_id = CT92_to_fao_trade %>% left_join(fao_trade_to_stocks, by = c("ID_FAOTrade", "Commodity"))
CT92_to_stock_match = CT92_to_stock_id %>% left_join(stocks, by = "Stock_Id")
CT92_to_actual_stocks = CT92_to_stock_match %>% left_join(stock, by = c("fao_code", "fao_area", "iso3", "country", "comm_name"))






#check = CT92_to_actual_stocks %>% select(Shortdescription.HS1992, comm_name, sci_name)%>%distinct()

#write.csv(check, "checkmatchingstocksHS92.csv")

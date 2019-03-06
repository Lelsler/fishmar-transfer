#clear workspace
rm(list = ls())
graphics.off()


setwd("/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/processed/timeseries")

require(dplyr)

#load required data
codes = read.csv("timeseries.needed.csv")
comtrade_1995 = read.csv("/Users/mtn1/Desktop/baci92_2016.csv")
hs_to_hs = read.csv("1992_to_2012.csv")
ct_to_fao = read.csv("ct_to_fao.csv")
trade_to_stocks = read.csv("trade_to_stocks.csv")
stocks = read.csv("stocks_match.csv")
stock = read.csv("/Users/mtn1/Nextcloud/FISHMAR-data/fao_stock_status/1950_2017_FAO_bbmsy_timeseries_merge.csv")
country_codes = read.csv("country_code_baci92.csv")
  

#extract
comtrade = comtrade_1995 %>% left_join(codes, by="hs6") 
comtrade = comtrade %>% filter(!is.na(comtrade$Code.HS1992))
list_ct_1995= unique(comtrade %>% select("Code.HS1992")) # get all comtrade 1992 fish codes that were traded in year of interest

#add HS2012 codes to this file to enable matching
extract_12 = list_ct_1995 %>% left_join(hs_to_hs, by="Code.HS1992")

#match HS2012 codes to the fao trade codes
extract_12_fao = extract_12 %>% left_join(ct_to_fao, by="Commodity.HS2012code")

trade_to_stocks = trade_to_stocks %>% select("Stock_Id", "ID_FAOTrade") %>% left_join(extract_12_fao, by = "ID_FAOTrade")

#match fao trade codes to fao stock codes 
stocks = stocks %>% select("Stock_Id", "stockid_CF") %>% rename(stockid = stockid_CF) %>%  left_join(trade_to_stocks, by="Stock_Id")

# match stocks with id's to Chris estimates
match_codes_stocks = stock %>% filter(year == unique(comtrade_1995$t)) %>% left_join(stocks, by = "stockid") %>% filter(!is.na(Code.HS1992))%>% 
  select(-Commodity.HS2012.y, -Commodity.HS2012.x, -Shortdescription.HS1992) 

#match country codes
comtrade_country = comtrade%>% left_join(country_codes,  by = "i")

# this keeps getting stuck on my computer, so I think we'll have to match the stocks to the exporting country here
df = comtrade_country %>% left_join(match_codes_stocks, by = c("Code.HS1992", "country", "iso3")) %>% filter(!is.na(stockid))


write.csv(df, "comtrade_1992_year2016_with_stocks.csv")#contains duplicates because multiple


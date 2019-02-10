#script to get all trade data for all the stocks that we have stock data from 
setwd("/Users/mtn1/Dropbox/BACI/")
library(dplyr) # you need to load the library every time you open R

trade <-data.frame()
for(x in c(1995:2016)){
  print("don't worry this is supposed to take long as these files are huge!")
  trade <- bind_rows(trade, read.csv(paste0('baci92_', x, '.csv'))) #read and bind each of the csv files
}

head(trade)

timeseries = "/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/processed/timeseries/"
stockwd = "/Users/mtn1/Nextcloud/FISHMAR-data/fao_stock_status/"

codes = read.csv(paste0(timeseries, 'timeseries.needed.csv'))
hs_to_hs = read.csv(paste0(timeseries,'1992_to_2012.csv'))
ct_to_fao = read.csv(paste0(timeseries,"ct_to_fao.csv"))
fao_trade_to_stocks = read.csv(paste0(timeseries,'trade_to_stocks.csv'))
stocks = read.csv(paste0(timeseries,'stocks_match.csv'))
stock = read.csv(paste0(stockwd, '1950_2017_FAO_bbmsy_timeseries_merge.csv'))
country_codes = read.csv(paste0(timeseries,'country_code_baci92.csv'))

CT92_to_fao_trade = ct_to_fao %>% left_join(hs_to_hs, by = c("Commodity.HS2012code","Commodity.HS2012"))
CT92_to_stock_id = CT92_to_fao_trade %>% left_join(fao_trade_to_stocks, by = c("ID_FAOTrade","Commodity"))
CT92_to_stock_match = CT92_to_stock_id %>% left_join(stocks, by = "Stock_Id")

#now we can get to which 92 stocks match to which FAO stock estimates from landings, and extract the landings data from which we have
#stock estimates:
CT92_to_actual_stocks = CT92_to_stock_match %>% left_join(stock, by = c("fao_code", "fao_area", "iso3", "country", "comm_name")) %>%
  select(Code.HS1992, Shortdescription.HS1992, iso3) %>% distinct() %>% filter(!is.na(iso3)) %>% left_join(country_codes)

#match now to the trade data and extract only info from stocks we have data on
trade = trade%>% rename(Code.HS1992 = hs6) %>% left_join(CT92_to_actual_stocks, by = c("Code.HS1992", "i")) %>%
  filter(!is.na(Shortdescription.HS1992))%>% distinct()

write.csv(trade, paste0(timeseries, 'CT_92_trade_thatmatchestostocks.csv'))
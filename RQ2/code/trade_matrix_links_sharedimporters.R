rm(list = ls())

library(dplyr)
library(igraph)


ct = readRDS("/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/raw/2012_comtrade/2012-16_comtrade_hs12_data_use.Rds")
stocks = read.csv("~/Nextcloud/FISHMAR-data/fao_stock_status/1950_2017_FAO_bbmsy_timeseries_merge.csv")

key = read.csv("/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/raw/2012_comtrade/hs6_fao_status_code_key.csv")
key = key %>% filter(Usage == "2_used") %>% rename(hs12code = Commodity.HS2012code)

trade = ct %>% left_join(key, by = "hs12code")
#crab = trade %>% filter(grepl("crab", Commodity)) %>% filter(Comm.Name!= "")
stocks$exp_iso3 = stocks$iso3

crab = trade %>% left_join(stocks,  by = c("year", "exp_iso3"))



crab = trade %>% filter(Comm.Name =="Blue king crab")
usa_crab = crab %>% filter(imp_iso3 == "USA")

to_be_matrix = usa_crab %>% select(exp_iso3, quantity_mt) %>% distinct() %>% filter(!is.na(quantity_mt) & exp_iso3!="NULL") %>%
  group_by(exp_iso3) %>%summarise(quantity = sum(quantity_mt))

#now local stocks are also linked through trade...


exp = unique(to_be_matrix$exp_iso3)

correlations <- data.frame( exp_iso3=character(), 
                            quantity=numeric(),
                            linked_exp=character(),
                            linked_exp_volume=numeric(),
                            stringsAsFactors=FALSE) 

colnames(correlations) <- c("exp_iso3", "quantity","linked_exp", "linked_exp_volume")


for (i in exp) {
  
  exp_temp = to_be_matrix[to_be_matrix$exp_iso3 == i,]
  correlation = to_be_matrix
  correlation$linked_exp = exp_temp$exp_iso3
  correlation$linked_exp_volume = exp_temp$quantity
  correlations = bind_rows(correlations, correlation)
  print(correlation)
}

correlations$sort_of_correlation = ifelse(correlations$quantity<correlations$linked_exp_volume, correlations$quantity, correlations$linked_exp_volume)

correlations = correlations %>% select(exp_iso3, linked_exp, sort_of_correlation) %>% filter(exp_iso3!= linked_exp)
correlations$sort_of_correlation = log(correlations$sort_of_correlation)

g = graph.data.frame(correlations, directed = FALSE)
E(g)$weight = correlations$sort_of_correlation

palf <- colorRampPalette(c("gold", "red"))

netm <- get.adjacency(g, attr="weight", sparse=F)
heatmap(netm, Rowv = NA, Colv = NA,
        scale="none", col = palf(100))

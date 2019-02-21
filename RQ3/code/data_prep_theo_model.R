# Clear workspace
rm(list = ls())
graphics.off() 

# libraries
library(readr)
library(dplyr)
library(tidyverse)

setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/")

stock_status <- read.csv("./mexico/processed/data_RQ3_filtered.csv")
monthly_effort <- read.csv("./mexico/processed/laura/RQ3_monthly_dataset_lge.csv")

# clean names 
setnames(stock_status, old=c("X...Coop_Id","Cooperative.name","Abalone","Clams","Lobsters","Sea.cucumber","Snails"), new=c("coop_id", "coop_name","abalone_stock","clam_stock","lobster_stock","seacucumber_stock","snail_stock"))

# select columns
monthly_effort_select <- monthly_effort %>% select(ID.x, Coop_Id, Cooperative_name, State.x,Confederacion.x,Federacion.x,functionality.x,year,
                          month,boats_abalone,boats_clams,boats_lobsters,
                          boats_seacucumber,boats_snails,catch_abalone,catch_clams,
                          catch_lobsters,catch_seacucumber,catch_snails,days_abalone,
                          days_clams,days_lobsters,days_seacucumber,days_snails,
                          value_abalone,value_clams,value_lobsters,value_seacucumber,
                          value_snails)

# set names 
setnames(monthly_effort_select, old=c("ID.x","Coop_Id","Cooperative_name","State.x","Confederacion.x","Federacion.x","functionality.x"), new=c("ID","coop_id", "coop_name","state","confederacion","federacion","functionality"))

# merge data sets 
stock_monthly <- left_join(monthly_effort_select,stock_status, by=c("coop_id","coop_name","year")) 

# calc new columns for reference price M for each species
# m_abalone = value_abalone/(ln(catch_abalone))

# calc new columns for harvest rate h for each species
# h_abalone = catch_abalone/stock_abalone

write.csv(stock_monthly, '~/laura/data_monthly_lge.csv',row.names=FALSE)



require(dplyr)
library(tidyr)
library(data.table)

### load required data
dta = read.csv("./mexico/processed/laura/data_monthly_lge.csv") 

### 
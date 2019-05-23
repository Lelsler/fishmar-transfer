# Clear workspace
rm(list = ls())
graphics.off() 

# libraries
library(readr)
#library(dplyr)
library(tidyverse)
library(plotly)
library(TTR)

setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/")

# read files
stock_status <- read.csv("./mexico/processed/data_RQ3_filtered.csv")
monthly_effort <- read.csv("./mexico/processed/laura/RQ3_monthly_dataset_lge.csv")
catchability <- read.csv("./mexico/processed/laura/coop_catchability_coeffecient.csv")

# clean names 
setnames(stock_status, old=c("X...Coop_Id","Cooperative.name","Abalone","Clams","Lobsters","Sea.cucumber","Snails"), new=c("coop_id", "coop_name","stock_abalone","stock_clams","stock_lobsters","stock_seacucumber","stock_snails"))

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
stock_monthly$m_abalone = (stock_monthly$value_abalone/log(stock_monthly$catch_abalone))
stock_monthly$m_clams = (stock_monthly$value_clams/log(stock_monthly$catch_clams))
stock_monthly$m_lobsters = (stock_monthly$value_lobsters/log(stock_monthly$catch_lobsters))
stock_monthly$m_seacucumber = (stock_monthly$value_seacucumber/log(stock_monthly$value_seacucumber))
stock_monthly$m_snails = (stock_monthly$value_snails/log(stock_monthly$catch_snails))

# calc new columns for harvest rate h for each species
stock_monthly$h_abalone = stock_monthly$catch_abalone/stock_monthly$stock_abalone
stock_monthly$h_clams = stock_monthly$catch_clams/stock_monthly$stock_clams
stock_monthly$h_lobsters = stock_monthly$catch_lobsters/stock_monthly$stock_lobsters
stock_monthly$h_seacucumber = stock_monthly$catch_seacucumber/stock_monthly$stock_seacucumber
stock_monthly$h_snails = stock_monthly$catch_snails/stock_monthly$stock_snails

# add catchability coefficients for each cooperative (I first did this after reading the csv file in case there is an error)
stock_monthly= left_join(x=stock_monthly, y = catchability, by= "coop_id",  all.x=TRUE) # stock_monthly, catchability

# calc new columns for effort-dependent cost for each species
stock_monthly$c_abalone = stock_monthly$days_abalone*stock_monthly$boats_abalone*stock_monthly$q_abalone
stock_monthly$c_clams = stock_monthly$days_clams*stock_monthly$boats_clams*stock_monthly$q_clams
stock_monthly$c_lobsters = stock_monthly$days_lobsters*stock_monthly$boats_lobsters*stock_monthly$q_lobsters
stock_monthly$c_seacucumber = stock_monthly$days_seacucumber*stock_monthly$boats_seacucumber*stock_monthly$q_seacucumber
stock_monthly$c_snails = stock_monthly$days_snails*stock_monthly$boats_snails*stock_monthly$q_snails

# save csv file
#write.csv(stock_monthly, './mexico/processed/laura/data_monthly_lge.csv',row.names=FALSE)

################################# for plotting #########################################################
# read csv
stock_monthly <- read.csv("./mexico/processed/laura/data_monthly_lge.csv")

plot(stock_monthly$m_abalone,stock_monthly$h_abalone)
plot(stock_monthly$m_clams,stock_monthly$h_clams)
plot(stock_monthly$m_lobsters,stock_monthly$h_lobsters)
plot(stock_monthly$m_seacucumber,stock_monthly$h_seacucumber)
plot(stock_monthly$m_snails,stock_monthly$h_snails)

library(ggplot2)
library(RColorBrewer)
setwd("/Users/lauraelsler/Documents/SESYNC/GIT/fishmar/RQ3/")

# figures for abalone
p <- ggplot(stock_monthly, aes(x=(m_abalone), y=log2(stock_abalone), col=functionality)) +
  geom_point() +
  theme_bw() +
  scale_color_gradientn(colours=brewer.pal(9, 'RdYlBu'), name="coop_id") +
  xlab("Reference price") + ylab("Stock abalone")
p

u <- ggplot(stock_monthly, aes(x=(m_abalone), y=log2(stock_abalone), col=functionality)) +
  geom_point() +
  theme_bw() +
  scale_color_gradientn(colours=brewer.pal(9, 'RdYlBu'), name="coop_id") +
  xlab("Marginal cost benefit") + ylab("Stock abalone")
u

ggsave("./figures/abalone_sm.png", plot = p)

v <- ggplot(stock_monthly, aes(x=m_abalone, y=log2(catch_abalone/stock_abalone), col=functionality)) +
  geom_point() +
  theme_bw() +
  scale_color_gradientn(colours=brewer.pal(9, 'RdYlBu'), name="coop_id") +
  xlab("Reference price") + ylab("Harvest rate abalone")
v

ggsave("./figures/abalone_fm.png", plot = v)

o <- ggplot(stock_monthly, aes(x=m_clams, y=stock_clams, col=functionality)) +
  geom_point() +
  theme_bw() +
  scale_color_gradientn(colours=brewer.pal(9, 'RdBu'), name="Functionality") +
  xlab("Reference price") + ylab("Stock clams")
o
ggsave(".figures/clams_sm.png", plot = o)


m <- ggplot(stock_monthly, aes(x=m_lobsters, y=stock_lobsters, col=functionality)) +
  geom_point() +
  theme_bw() +
  scale_color_gradientn(colours=brewer.pal(9, 'RdBu'), name="Functionality") +
  xlab("Reference price") + ylab("Stock lobster")
m
ggsave(".figures/lobsters_sm.png", plot = m)


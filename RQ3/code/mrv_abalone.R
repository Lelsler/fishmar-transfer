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
stock_monthly <- read.csv("./mexico/processed/laura/data_monthly_lge.csv")

# subset to calculate marginal revenue
ssv <- c("value_abalone", "catch_abalone", "days_abalone","h_abalone","m_abalone")
sst <- stock_monthly[ssv]
sst <- na.omit(sst)
sst <- sst[order(sst$value_abalone),]

# moving average value and catch (to reduce fluctuations in the calculation of )
sst$mva_value_abalone <- SMA(sst$value_abalone,n=10)
sst$mva_catch_abalone <- SMA(sst$catch_abalone,n=10)

# calculate marginal revenue 
for(i in 10:length(sst$value_abalone)) {
  # calculate marginal revenue  
  sst$mrv_abalone[i] = (sst$mva_value_abalone[i]-sst$mva_value_abalone[i-1])/(sst$mva_catch_abalone[i]-sst$mva_catch_abalone[i-1])
}


### plot
library(ggplot2)
library(RColorBrewer)
setwd("/Users/lauraelsler/Documents/SESYNC/GIT/fishmar/RQ3/")

k <- ggplot(sst, aes(x=catch_abalone, y=mrv_abalone), col=days_abalone) +
  geom_point() +
  theme_bw() +
  scale_color_gradientn(colours=brewer.pal(9, 'RdBu'), name="Functionality") +
  xlab("Catch abalone") + ylab("Marginal revenue abalone")
k
k + scale_y_continuous(trans='log10') # need to save seperately
ggsave("./figures/abalone_mrv.png", plot = k)


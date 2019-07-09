# Clear workspace
rm(list = ls())
graphics.off() 

# libraries
library(readr)
library(tidyverse)
library(plotly)
library(TTR)
library(ggplot2)
library(RColorBrewer)

setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/")
# read csv
stock_monthly <- read.csv("./mexico/processed/laura/data_monthly_lge.csv")

################################# for plotting #########################################################
setwd("/Users/lauraelsler/Documents/SESYNC/GIT/fishmar/RQ3/")

# figures for abalone
p <- ggplot(stock_monthly, aes(x=(m_abalone), y=log2(stock_abalone/Bmsy_abalone), col=functionality)) +
  geom_point() +
  theme_bw() +
  scale_color_gradientn(colours=brewer.pal(9, 'RdYlBu'), name="coop_id") +
  xlab("Reference price") + ylab("B/Bmsy abalone")
p
#ggsave("./figures/abalone_sm.png", plot = p)

v <- ggplot(stock_monthly, aes(x=m_clams, y=log2(stock_clams/Bmsy_clams), col=functionality)) +
  geom_point() +
  theme_bw() +
  scale_color_gradientn(colours=brewer.pal(9, 'RdYlBu'), name="coop_id") +
  xlab("Reference price") + ylab("B/Bmsy clams")
v
#ggsave("./figures/clams_sm.png", plot = v)

o <- ggplot(stock_monthly, aes(x=m_lobsters, y=log2(stock_lobsters/Bmsy_lobsters), col=functionality)) +
  geom_point() +
  theme_bw() +
  scale_color_gradientn(colours=brewer.pal(9, 'RdBu'), name="Functionality") +
  xlab("Reference price") + ylab("B/Bmsy lobsters")
o
#ggsave(".figures/lobsters_sm.png", plot = o)

m <- ggplot(stock_monthly, aes(x=m_seacucumber, y=log2(stock_seacucumber/Bmsy_seacucumber), col=functionality)) +
  geom_point() +
  theme_bw() +
  scale_color_gradientn(colours=brewer.pal(9, 'RdBu'), name="Functionality") +
  xlab("Reference price") + ylab("B/Bmsy sea cucumber")
m
#ggsave(".figures/seacucumber_sm.png", plot = m)

n <- ggplot(stock_monthly, aes(x=m_snails, y=log2(stock_snails/Bmsy_snails), col=functionality)) +
  geom_point() +
  theme_bw() +
  scale_color_gradientn(colours=brewer.pal(9, 'RdBu'), name="Functionality") +
  xlab("Reference price") + ylab("B/Bmsy snails")
n
#ggsave(".figures/snails_sm.png", plot = n)


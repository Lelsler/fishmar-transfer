# Clear workspace
rm(list = ls())
graphics.off() 

# libraries
library(readr)
library(plotly)
library(ggplot2)
library(RColorBrewer)

# read data
setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/")
stock_monthly <- read.csv("./mexico/processed/laura/data_monthly_lge.csv")

### FIGURES
setwd("/Users/lauraelsler/Documents/SESYNC/GIT/fishmar/RQ3/")

savePlot <- function(pu) {
  pdf("./figures/abalone_sm")
  print(pu)
  dev.off()
}
  
### abalone
# bbmsy
pu <- ggplot(ggplot(data=stock_monthly, aes(x=(m_abalone), y=log2(stock_abalone), col=functionality)) +
  geom_point() +
  theme_bw() +
  scale_color_gradientn(colours=brewer.pal(9, 'RdYlBu'), name="coop_id") +
  xlab("Reference price") + ylab("Stock abalone"))

savePlot(pu) 

ggsave("./figures/abalone_sm.png", plot = p)


ggsave(filename = "./figures/abalone_fm.png", plot = v)

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


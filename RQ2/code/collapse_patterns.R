# 23/Sept/2018
# Script to identify temporal and spatial patterns in stock collapses
# --
rm(list = ls())
detach("package:dplyr",unload = T)
# Set working directoy to home
setwd("/research-home/agiron/fishmar")

# Define functions
# Moving average
#library(dplyr)
ma <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}

# Read data
data <- read.csv('/nfs/FISHMAR-data/fao_stock_status/1950_2017_FAO_bbmsy_timeseries_merge.csv')

# Create extra column to store smooth ts
Sma = rep(NA,NROW(data))
data = data.frame(data,Sma)

# Identify different stocks
stocks = as.character(unique(data$stockid))

# Step 1: Loop to calculate moving averages for each time series (smooth)
for (i in 1:length(stocks)){
  stock.id = which(data$stockid == stocks[i])
  S = data$super[stock.id] # Extract stock time series
  data$Sma[stock.id] = ma(S,5) # Moving average to smooth
}

# Step 2: Loop to evaluate collapsed stocks
# Limit for collapse is B/Bmsy < 0.5
data$collapsed = rep(0,NROW(data))
data$collapsed[data$Sma < 0.5] = 1
L = rep(NA,NROW(stocks)) # Empty vector to register year of first collapse and start of decadent trend
R = data.frame(stocks,year_collapse = L, year_start = L)
# Step 3: evaluate the duration of collapse
for (i in 1:length(stocks)){
  stock.id = which(data$stockid == stocks[i])
  S = data[stock.id,c(10,20,21)] # Extract stock time series
  plot(S$year,S$Sma,type = 'l',ylim = c(0,2))
  title(paste(i,',',stocks[i]))
  abline(h = 0.5,lty = 2,col = 'red')
  id.c = which(S$collapsed == 1) # Id of the first collapse
  if(length(id.c) > 0){ # If there was a collapse ...
    id.c = min(id.c)
    points(S$year[id.c],S$Sma[id.c]) # Plot the collapsed point in the graph
    R$year_collapse[i] = S$year[id.c] # Store the collapse year
    R$year_start[i] <- round(as.numeric(locator(n = 1)))[1] # Select the year of the start of collapse
  }
}

R$duration = R$year_collapse - R$year_start

write.csv(R,'/nfs/FISHMAR-data/fao_stock_status/ts_collapses_analysis.csv')


# # Count the number of collapsed stocks per year
# library(dplyr)
# collapseTS = data %>% 
#   group_by(year) %>% 
#   summarise(Collapsed = sum(collapsed))
# 
# collapseTS = collapseTS[-(65:66),]
# 
# plot(collapseTS$year, collapseTS$Collapsed,type = 'l',
#      xlab = "Year",ylab = "Freq.",
#      main = '# of collapsed stocks (B/Bmsy < 0.5)')
# mtext(side = 3,line = 0.35,paste('n =',length(stocks)))

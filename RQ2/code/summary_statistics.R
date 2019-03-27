#The data set is called "fish".

#clear workspace
rm(list = ls())
graphics.off()

#setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/")

library(tidyverse)
library(data.table)
require(readxl)

### load required data
duration= read_excel("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/trade_duration.xlsx")

### new names
setnames(duration, old = c("\ufeffGroupName",'Max.Duration', 'Sum. Duration','Avg. Duration'), new = c('group_name','max_duration', 'sum_duration','avg_duration'))

### calculate mean 
duration_spp <- aggregate(duration$max_duration, by=list(duration=duration$group_name), FUN=mean)
duration_spp$sum.duration <- aggregate(duration$sum_duration, by=list(duration=duration$group_name), FUN=mean)
duration_spp$avg.duration <- aggregate(duration$avg_duration, by=list(duration=duration$group_name), FUN=mean)

# load other data
duration= read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/trade_duration_sppgroup.csv")
volume= read_excel("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/summary_table_sppgroup_statistics.xlsx")
collapse= read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/trade_collapse_sppgroup.csv")
sto = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/fao_stock_status/1950_2017_FAO_bbmsy_timeseries_merge.csv") 
mat1 = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/timeseries/matchingstocksHS92.csv") # HS92 (from trade) matched to comm_name, sci_name (stock status file)
mat2 = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/trade_groups_for_new_links92.csv") 

# rename column in files # show files with head(mat1)
colnames(mat1)[colnames(mat1)=="Shortdescription.HS1992"] <- "HS92" # an alternative way of reanming is # mat1 = mat1 %>% rename(HS92 = Shortdescription.HS1992)
colnames(mat2)[colnames(mat2)=="X...Shortdescription.HS1992"] <- "HS92" 
colnames(mat2)[colnames(mat2)=="group_name"] <- "our_group_name" 

# join files by columns
mat1= left_join(x=mat1, y = mat2, by= "HS92",  all.x=TRUE) # mat1 mat2

# clean mat1: only two columns, delete duplicates, omit na´s
mat3 = mat1 %>% distinct() %>% select(sci_name, our_group_name)
mat3 = unique(mat3)
mat3 = na.omit(mat3)

# match the key to spp groups and stock values 
p = left_join(x=sto, y = mat3) 

# calculate the mean per spp group
stock <- aggregate(super ~ our_group_name, p, mean)
setnames(stock, old=c('our_group_name'), new=('group_name'))

### new names
setnames(duration, old = c('x','duration','sum.duration.x','avg.duration.x'), new = c('max_duration', 'group_name','sum_duration','avg_duration'))
setnames(volume, old = c('Metric/spp group',"Trade volume trend 1995-2016", "Trade volume average increase per year 1995-2016", "Trade volume stdv 1995-2016"), new = c('group_name','trend','avg_increase','stdv_increase'))

### compose data
summary = left_join(x=collapse, y=duration[ , c("group_name","max_duration", "sum_duration", "avg_duration")], all.x=TRUE) 
summary = left_join(x=summary, y=volume[ , c('group_name','trend','avg_increase','stdv_increase')], all.x=TRUE) 
summary = left_join(x=summary, y=stock[ , c('group_name','super')], all.x=TRUE) 

### save data
write.csv(summary, '/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/summary_statistics.csv')

### plot
# simple scatter plots of trade collapse
plot(summary$trade_collapse, summary$super, main="Correlations summary statistics",
     xlab="trade collapse", ylab="stock status", pch=19) 
abline(lm(summary$trade_collapse~summary$super), col="red") # regression line (y~x)
lines(lowess(summary$trade_collapse,summary$super), col="blue") # lowess line (x,y) 

plot(summary$relative_trade_collapse, summary$super, main="Correlations summary statistics",
     xlab="relative trade collapse", ylab="stock status", pch=19) 
abline(lm(summary$relative_trade_collapse~summary$super), col="red") # regression line (y~x)
lines(lowess(summary$relative_trade_collapse,summary$super), col="blue") # lowess line (x,y) 

plot(summary$avg_duration, summary$super, main="Correlations summary statistics",
     xlab="avg trade duration", ylab="stock status", pch=19) 
abline(lm(summary$avg_duration~summary$super), col="red") # regression line (y~x)
lines(lowess(summary$avg_duration,summary$super), col="blue") # lowess line (x,y) 

plot(summary$trend, summary$super, main="Correlations summary statistics",
     xlab="trade volume trend", ylab="stock status", pch=19) 
#abline(lm(summary$trend~summary$super), col="red") # regression line (y~x)
#lines(lowess(summary$trend,summary$super), col="blue") # lowess line (x,y) 

plot(summary$avg_increase, summary$super, main="Correlations summary statistics",
     xlab="trade volume trend year-to-year", ylab="stock status", pch=19) 
#abline(lm(summary$avg_increase~summary$super), col="red") # regression line (y~x)
#lines(lowess(summary$avg_increase,summary$super), col="blue") # lowess line (x,y) 



# multi-panel scatter
# library(ggplot2)
# library(gridExtra)
# 
# grid.arrange(
#   ggplot(summary, aes(relative_trade_collapse, super, colour=group_name)) +
#     geom_point() +
#     geom_smooth(alpha=0.2) +
#     theme(legend.position="top"),
#   ggplot(summary, aes(trade_collapse, super, colour=group_name)) +
#     geom_point() +
#     geom_smooth(alpha=0.2),
#   theme(legend.position="top"),
#   ncol=2)







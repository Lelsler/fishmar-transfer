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


# write csv
#write.csv(duration_spp, "/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/trade_duration_sppgroup.csv")

# load other data
volume= read_excel("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/summary_table_sppgroup_statistics.xlsx")
collapse= read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/trade_collapse_sppgroup.csv")

### new names
setnames(duration_spp, old = c('x','duration','sum.duration','avg.duration'), new = c('max_duration', 'group_name','sum_duration','avg_duration'))
setnames(volume, old = c('Metric/spp group',"Trade volume trend 1995-2016", "Trade volume average increase per year 1995-2016", "Trade volume stdv 1995-2016"), new = c('group_name','trend','avg_increase','stdv_increase'))

### compose data
summary = left_join(x=collapse, y=duration_spp[ , c("group_name","max_duration", "sum_duration", "avg_duration")], all.x=TRUE) 
summary = left_join(x=summary, y=volume[ , c('group_name','trend','avg_increase','stdv_increase')], all.x=TRUE) 

### 

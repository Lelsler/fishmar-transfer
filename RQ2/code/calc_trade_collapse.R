#The data set is called "fish".

#clear workspace
rm(list = ls())
graphics.off()

setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/")

library(tidyverse)
library(data.table)

### load required data
# bilateral trade data
fish = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/timeseries/CT_fish_trade92.csv") 

#create new variable "group" by combining the names of the two countries trading and the species
fish$group <- paste(fish$iso3,fish$imp.iso3,fish$group_name, sep = "-")

#Sort the data set by the created groups (if you want)
fish<- fish %>% group_by(group)

#All the the new groups (every combination on countries and species)
list(fish$group)

#Aggregate the groups to one observation per group based on the max value of "trade", in a new data set
fish.trade.max <- aggregate(fish$q, by = list(fish$group), max)

#Rename the variables
names(fish.trade.max) <- c("group", "q")

#Add year of maximum trade
fish.trade.max= left_join(x=fish.trade.max, y = fish[ , c("q","group","t")], all.x=TRUE) 

#Rename columns 
setnames(fish.trade.max, old = c('q','t'), new = c('q_max','t_max'))
fish.trade.max <- na.omit(fish.trade.max)

#Add max year and quantity to fish data set
fish = left_join(x=fish, y = fish.trade.max[ , c("group","t_max","q_max")], all.x=TRUE) 

#New column for fish collapse
fish$collapse <- NA

#Calculate trade collapse i.e. < 10% of maximum traded volume
fish$collapse = ifelse(fish$t > fish$t_max & fish$q/fish$q_max < 0.1, 1, 0) 

#Save to csv file
#write.csv(fish, "/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/trade_collapse.csv")

#Count trade collapses per spp group
fish.collapse <- aggregate(fish$collapse,by = list(fish$group_name), FUN=sum, na.rm=TRUE, na.action=NULL)
setnames(fish.collapse, old = c('Group.1','x'), new = c('group_name','trade_collapse'))

#Total number of trades (per country pair & per year) 
p <- fish %>% group_by(group_name) %>%
  summarise(total_trades = length(unique(q)))
fish.collapse = left_join(x=fish.collapse, y = p[ , c("group_name","total_trades")], all.x=TRUE) 
  
#Calculate relative frequency of collapse
fish.collapse <-mutate(fish.collapse, relative_trade_collapse= total_trades/trade_collapse)

#Save to csv file
#write.csv(fish.collapse, "/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/trade_collapse_sppgroup.csv")




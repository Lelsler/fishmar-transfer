rm(list = ls())

setwd("/Users/mtn1/Nextcloud/FISHMAR-data/rq2/test_preferential/")

# libraries
require(igraph)
require(dplyr)

stocks = read.csv("~/Nextcloud/FISHMAR-data/fao_stock_status/1950_2017_FAO_bbmsy_timeseries_merge.csv")
trade = read.csv("~/Nextcloud/FISHMAR-data/comtrade/processed/timeseries/CT_fish_trade92.csv")
match = read.csv("~/Nextcloud/FISHMAR-data/comtrade/processed/timeseries/matchingstocksHS92.csv")

match = match[, 1:3]
match = na.omit(match)# we need to look at the non-matches (tuna needs to have a match for instance)


trade = trade %>% left_join(match, by = "Shortdescription.HS1992") %>% rename(year=t)
trade_stocks  = stocks %>% left_join(trade, by = c("iso3", "sci_name", "comm_name", "year"))
# I have to look at this file later because things went wrong with the matching, for instance stock atlantic chub mackarel doesn't have a trade friend 
# so look at all NA's for stocks that don't have matching trade

#but delete them for now:

trade_stocks = trade_stocks %>% filter(!is.na(group_name))

#look at only eels:

trade_stocks = trade_stocks %>% filter(group_name=="eel")

#calculate unweighted average stock status per country per year
network_eel = trade_stocks %>% group_by(year, iso3) %>% summarise(mean_super = mean(super)) 
#this is a bit backwards but select trade and re-aggregate with mean stock status
eel_trade = trade_stocks  %>% select(iso3, imp.iso3, v, year) %>% distinct() %>% group_by(iso3, imp.iso3, year) %>% summarise(sum_trade = sum(v))
network_eel = eel_trade %>% left_join(network_eel, by = c("iso3", "year"))%>% filter(sum_trade >10)

importers = network_eel %>% select(year, imp.iso3, sum_trade)
importers =importers[with(importers, order(year, -sum_trade)), ]
importers = importers[, 2:4]

year = unique(importers$year)



biggest.importers <- data.frame(year = integer(),
                               imp.iso3=character(),
                               sum_trade = numeric(),
                               ID= integer(),
                               stringsAsFactors=FALSE) 

colnames(biggest.importers) <- c("year", "imp.iso3", "sum_trade", "ID") # Because the column names didn't work right.

for (i in min(year):max(year)){
  
  importers.temp = subset(importers, year == i)
  importers.temp$ID = c(1:length(importers.temp$imp.iso3))
  importers.temp <- importers.temp[1:3,]
  print(importers.temp)
  
  biggest.importers = bind_rows(biggest.importers, importers.temp)
  }


biggest = data.frame(unique(biggest.importers$imp.iso3))
colnames(biggest)= c("imp.iso3")
biggest$biggest = "biggest"

biggest = biggest %>% filter(imp.iso3!="NULL")

network_eel_five = network_eel %>% left_join(biggest)
network_eel_five = network_eel_five %>% filter(!is.na(biggest))

years = unique(network_eel_five$year)



#plot network for each year t from start.year to end.year
plotT <- function(t) {
  
  network_eel_t = network_eel_five %>% filter(year==t) %>% select(iso3, imp.iso3, sum_trade, mean_super)
  
  edgelist = network_eel_t%>%select(iso3, imp.iso3, sum_trade)
  attribute = network_eel_t %>% select(iso3, mean_super)%>% distinct()
  
  #this may not work very well
  df = data.frame(unique(edgelist$imp.iso3))
  colnames(df) = "iso3"
  attribute = merge(attribute, df, all=TRUE)
  attribute$mean_super[is.na(attribute$mean_super)] = 0
  attribute$type = ifelse(attribute$mean_super < 0.75, "overexploited", "sustainable")
  attribute$type = ifelse(attribute$mean_super == 0, "only importing", attribute$type)
  
  gf = graph_from_data_frame(edgelist, directed = TRUE)
  
  
  nodeList = data.frame(Name = igraph::V(gf)$name, 
                        ID = c(0:(igraph::vcount(gf) - 1))) # 
  nodeDegree=igraph::degree(gf, v = igraph::V(gf), mode = "all")
  nodeList = cbind(nodeList, nodeDegree, i)
  nodeList$iso3 = nodeList$Name
  
  
  attribute = network_eel_t %>% select(iso3, mean_super)%>% distinct()
  attribute = attribute[, 2:3]
  attribute = attribute %>% distinct()
  #this may not work very well
  df = data.frame(unique(edgelist$imp.iso3))
  colnames(df) = "iso3"
  attribute = merge(attribute, df, all=TRUE)
  
  attribute$mean_super[is.na(attribute$mean_super)] = 0
  attribute$type = ifelse(attribute$mean_super < 0.75, "overexploited", "sustainable")
  attribute$type = ifelse(attribute$mean_super == 0, "only importing", attribute$type)
  
  attribute = attribute %>% distinct()
  nodeList = nodeList %>% left_join(attribute)
  
  E(gf)$weight <- edgelist$sum_trade
  V(gf)$size <- nodeList$mean_super*10
  V(gf)$color=  nodeList$type #assign the "type" attribute as the vertex color
  V(gf)$shape = nodeList$type
  
  V(gf)$color=gsub("overexploited","red",V(gf)$color) #overexploited will be red
  V(gf)$color=gsub("sustainable","light green",V(gf)$color) #
  V(gf)$color=gsub("only importing","blue",V(gf)$color) 
  V(gf)$shape=gsub("only importing","square",V(gf)$shape)#the size of only importing needs to be different than currently
  V(gf)$shape=gsub("sustainable","circle",V(gf)$shape) 
  V(gf)$shape=gsub("overexploited","circle",V(gf)$shape) #overexploited will be red
  
  l = layout_in_circle(gf) # map the network on a circle
 
  xlim <- range(l[,2])
  ylim <- range(l[,2])
  
  
  
  plot(gf, layout = l, edge.width=E(gf)$weight/1000, edge.arrow.size=.2, edge.color = rgb(0,0,0,0.5), line=-0.5,
       vertex.label.color= "black", vertex.label.cex=0.7, xlim=xlim, ylim=ylim)
  
  title(main=t)
}

#plotting parameters
pdf('eel_networks.pdf',height = 4,width = 4)
par(mfrow=c(1,1),mar=c(1,1,1,1))# 

for (i in min(years):max(years)){

  plotT(i)
}
dev.off()


# is NULL in the raw data iso codes??

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

#look at only crabs:

trade_stocks = trade_stocks %>% filter(group_name=="crab")

#calculate unweighted average stock status per country per year
#network_crab = trade_stocks %>% filter(comm_name == "Blue swimming crab") %>% select(year, super, iso3)
network_crab = trade_stocks %>% group_by(year, iso3) %>% summarise(mean_super = mean(super)) 
#this is a bit backwards but select trade and re-aggregate with mean stock status
crab_trade = trade_stocks  %>% select(iso3, imp.iso3, v, year) %>% distinct() %>% group_by(iso3, imp.iso3, year) %>% summarise(sum_trade = sum(v))
network_crab = crab_trade %>% left_join(network_crab, by = c("iso3", "year"))%>% filter(sum_trade >10)

importers = network_crab %>% select(year, imp.iso3, sum_trade)
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
  importers.temp <- importers.temp[1:5,]
  print(importers.temp)
  
  biggest.importers = bind_rows(biggest.importers, importers.temp)
  }


biggest = data.frame(unique(biggest.importers$imp.iso3))
colnames(biggest)= c("imp.iso3")
biggest$biggest = "biggest"

biggest = biggest %>% filter(imp.iso3!="NULL")

network_crab_five = network_crab %>% left_join(biggest)
network_crab_five = network_crab_five %>% filter(!is.na(biggest))

years = unique(network_crab_five$year)



#plot network for each year t from start.year to end.year
plotT <- function(t) {
  
  network_crab_t = network_crab_five %>% filter(year==t) %>% select(iso3, imp.iso3, sum_trade, mean_super)
  
  edgelist = network_crab_t%>%select(iso3, imp.iso3, sum_trade)
  attribute = network_crab_t %>% select(iso3, mean_super)%>% distinct()
  
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
  
  
  attribute = network_crab_t %>% select(iso3, mean_super)%>% distinct()
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
  V(gf)$color=gsub("sustainable","light green",V(gf)$color) #overexploited will be red
  V(gf)$color=gsub("only importing","blue",V(gf)$color) #overexploited will be red
  V(gf)$shape=gsub("only importing","square",V(gf)$shape) #overexploited will be red
  V(gf)$shape=gsub("sustainable","circle",V(gf)$shape) #overexploited will be red
  V(gf)$shape=gsub("overexploited","circle",V(gf)$shape) #overexploited will be red
  
  l = layout_in_circle(gf) # map the network on a circle
 
  xlim <- range(l[,2])
  ylim <- range(l[,2])
  
  
  edge.width=E(gf)$weight/2000
  plot(gf, layout = l, edge.arrow.size=.2, edge.color = rgb(0,0,0,0.5), line=-0.5,
       vertex.label.color= "black", vertex.label.cex=0.7, xlim=xlim, ylim=ylim)
  
  title(main=t)
}

#plotting parameters
#pdf('crab_networks.pdf',height = 4,width = 4)
par(mfrow=c(1,1),mar=c(1,1,1,1))# 

for (i in min(years):max(years)){

  plotT(i)
}
#dev.off()


# is NULL in the raw data iso codes??

i = 2000
#degrees <- function(t) {
  
network_crab_t = network_crab %>% filter(year==i) %>% select(iso3, imp.iso3, sum_trade, mean_super)
edgelist = network_crab_t %>% select(iso3, imp.iso3, sum_trade)
g = graph_from_data_frame(edgelist, directed = TRUE)
nodeList = data.frame(Name = igraph::V(g)$name, 
                       ID = c(0:(igraph::vcount(g) - 1))) # 
nodeList = cbind(nodeList, nodeDegree=igraph::degree(g, v = igraph::V(g), mode = "all"), i)
nodeList = cbind(nodeList, nodeDegree.in=igraph::degree(g, v = igraph::V(g), mode = "in")) #calculate node degree for all nodes in network
nodeList = cbind(nodeList, nodeDegree.out=igraph::degree(g, v = igraph::V(g), mode = "out")) #calculate node degree for all nodes in network
nodeList = cbind(nodeList, weighted.close=igraph::betweenness(g, v = igraph::V(g), normalized = TRUE, directed = TRUE, weights =edgelist$sum_trade)) #calculate node degree for all nodes in network
nodeList = cbind(nodeList, unweighted.close=igraph::betweenness(g, v = igraph::V(g), normalized = TRUE, directed = TRUE)) #calculate node degree for all nodes in network


data = Reduce(rbind, unweighted_eigen_centrality)

unweighted_eigen_centrality=igraph::eigen_centrality(g, directed = TRUE, options = arpack_defaults)[1]



attribute = network_crab_t %>% select(iso3, mean_super)%>% distinct()
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
#}



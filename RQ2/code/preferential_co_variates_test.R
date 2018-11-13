# Clear workspace
rm(list = ls())
graphics.off() 

# for now this code is only looking at net increases and decreases in node-degree, while we are actually interested 
# in all the new connections that are formed. 

# libraries
require(igraph)
require(dplyr)
require(reshape2)

#setwd("/Users/mtn1/Dropbox/SFI/Before_November/ORIGINAL_DATA_FILES")
setwd("/Users/mtn1/Nextcloud/FISHMAR-data/rq2/test_preferential/")
# choose year & species
Rawdata.1995 <- read.csv("/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/processed/timeseries/comtrade_1992_year1995_with_stocks.csv")
Rawdata.1996 <- read.csv("/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/processed/timeseries/comtrade_1992_year1996_with_stocks.csv")
Rawdata.1997 <- read.csv("/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/processed/timeseries/comtrade_1992_year1997_with_stocks.csv")

data = rbind(Rawdata.1995, Rawdata.1996, all=TRUE)

data = rawdata %>% filter(t == 1995 | t == 1996)  # and other years that we add.. 

#what data we need
CountryChanges <- data.frame(country=character(),
                             year=character(), 
                             import=integer(),
                             export=integer(),
                             stringsAsFactors=FALSE) 
colnames(CountryChanges) <- c("Country", "Year", "NewImports", "NewExports") # Because the column names didn't work right.
#fish species code

years <- unique(data$t) # pull the unique set of years from the input file
countries <- unique(data$iso3) # 260 Countries in Importer or Exporter

x <- length(years)
x <- x-1 # this said x- 2 but think it should be minus 1.. so changed it 
#length(years) <- x # all years but the last year

for (t in years) { 
  
  # create appropriate first table, and first edgelist
  network.table1 <- data[data$t == t, ]
  edgeList1 <- data.frame(network.table1)
  
  #construct first network
  tradenetwork1 <- igraph::simplify(igraph::graph.data.frame(network.table1, directed=TRUE))
  nodeList1 <- data.frame(Name = igraph::V(tradenetwork1)$name) # create nodelist from tradenetwork
  nodeList1 <- cbind(nodeList1, nodeDegree1=igraph::degree(tradenetwork1 , v = igraph::V(tradenetwork1), mode = "all")) #calculate node degree for all nodes in network
  
  nextYear <- StartYear + 1
  # create appropriate second table, and first edgelist
  network.table2 <- Mydata[Mydata$Commodity.Code == Cod, ]
  network.table2 <- network.table2[network.table2$Year == nextYear, ]
  edgeList2 <- data.frame(network.table2)
  
  
  #construct second network
  tradenetwork2 <- igraph::simplify(igraph::graph.data.frame(network.table2, directed=TRUE))
  nodeList2 <- data.frame(Name = igraph::V(tradenetwork2)$name) # create nodelist from tradenetwork
  nodeList2 <- cbind(nodeList2, nodeDegree2=igraph::degree(tradenetwork2 , v = igraph::V(tradenetwork2), mode = "all")) #calculate node degree for all nodes in network
  
  ###### New Links
  for (country in countries) {
    c2 <- network.table2[network.table2$Exporter == country, ]
    c1 <- network.table1[network.table1$Exporter == country, ]
    
    comp <- c2$Importer %in% c1$Importer # New Imports
    Imp <- sum(comp, na.rm = TRUE)
    
    c2 <- network.table2[network.table2$Importer == country, ]
    c1 <- network.table1[network.table1$Importer == country, ]
    
    comp <- c2$Exporter %in% c1$Exporter # New Exports
    Exp <- sum(comp, na.rm = TRUE)
    
    NewRow <- c(country, StartYear+1, Imp, Exp)
    
    CountryChanges <- rbind(CountryChanges, NewRow)
    
  }
  ###### End 
  # calculate the frequency that a node with a certain degree is occuring, for each node
  reshape <- dcast(nodeList1, Name ~ nodeDegree1, value.var = 'nodeDegree1')
  o <- unique(nodeList1$nodeDegree1)
  d=sapply(unique(nodeList1$nodeDegree1),function(x){length(which(nodeList1$nodeDegree1==x))})
  a = data.frame (o, d)
  nodeList1$frequency <- sapply(nodeList1$nodeDegree1, function(x){a$d[which(x==a$o)]})
  
  # merge the two dataframes 
  bla2 <- merge(nodeList1, nodeList2, all=TRUE)
  
  #this only calculate the net increase and decrease in node-degree
  bla2$new.links = bla2$nodeDegree2-bla2$nodeDegree1
  
  bla2$new.links = ifelse(bla2$new.links > 0, bla2$new.links, 0) # make sure numbers are not negative
  bla2$new.links[is.na(bla2$new.links)] =0 # remove NA's
  
  #total net increase in links for countries that have a positive increase in links 
  total.new.links= bla2 %>% group_by(nodeDegree1) %>% summarise(degree.sum = sum(new.links, na.rm=T))
  
  bla2 = bla2 %>% left_join(total.new.links, by=nodeDegree1)
  
  bla2 = merge(bla2, total.new.links, all=TRUE)
  
  bla2 = bla2 %>% select('nodeDegree1', 'frequency', 'degree.sum')
  bla2 = unique(bla2)
  Rk = subset(bla2, !is.na(nodeDegree1) & degree.sum>0)
  
  
  #( # new links added to nodes of degree n / total # of new links of new links in the network )
  
  ## number is the sum of 
  
  number = sum(Rk$degree.sum)
  
  Rk$pk = (Rk$degree.sum / number)  
  
  Rk$rk = Rk$pk / (Rk$frequency / length(nodeList1)) 
  
  
  
  require(ggplot2)
  rkplot = ggplot(Rk, aes(x = nodeDegree1, y = rk)) + geom_point() + scale_y_continuous(trans='log')
  rkplot = rkplot + labs(title="test preferential attachment", y = "relative probability", x = "nodedegree")
  
  
  plot(log(bla2$nodeDegree1[bla2$probability>0]), log(bla2$new.links[bla2$probability>0]), main="pref attachment?" ,xlab="nodeDegree t1", ylab="new links t2", pch=19)
  
  linear <- lm(log(bla2$nodeDegree1[bla2$new.links>0]) ~ log(bla2$new.links[bla2$new.links>0]))
  
  summary(linear)
  
}#End For



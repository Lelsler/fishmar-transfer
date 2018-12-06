# Clear workspace
rm(list = ls())
graphics.off() 

# for now this code is only looking at net increases and decreases in node-degree, while we are actually interested 
# in all the new connections that are formed. 

# libraries
require(igraph)
require(dplyr)
require(reshape2)

# choose year & species
Rawdata.1995 <- read.csv("comtrade_1992_year1995_with_stocks.csv")
Rawdata.1996 <- read.csv("comtrade_1992_year1996_with_stocks.csv")
Rawdata.1997 <- read.csv("comtrade_1992_year1997_with_stocks.csv")
Rawdata.1998 <- read.csv("comtrade_1992_year1998_with_stocks.csv")

country_codes = read.csv("country_code_baci92.csv")

#is it the same?
colnames(country_codes) = c("imp.country", "imp.iso2", "imp.iso3", "j")

data1 = bind_rows(Rawdata.1995, Rawdata.1996, Rawdata.1997, Rawdata.1998) #gives warnings that don't matter
data1 = data1%>% left_join(country_codes,  by = "j")

data1 = data1 %>% select(t, Code.HS1992, Commodity, iso3, imp.iso3) 
data1 = unique(data1) #to get rid of the double counting

#logfile with in and out trades
logfile = "new_links.csv"

#Columns
cat("country, year, new imports, new exports, commodity", file=logfile, append=FALSE, sep = "\n")

commodities = unique(data1$Commodity)

#all new links
nl = read.csv("new_links.csv") #this works only if commas in orignial file are removed

# example for the preferential attachement
  network.table1 <- data1[data1$t == 1995 & data1$Commodity == "Anchovies fresh or chilled", ]
  edgeList1 <- data.frame(network.table1$iso3, network.table1$imp.iso3)
  colnames(edgeList1) = c("Exporter","Importer")

#construct network
  tradenetwork1 <- igraph::simplify(igraph::graph.data.frame(edgeList1, directed=TRUE))
  nodeList1 <- data.frame(Name = igraph::V(tradenetwork1)$name) # create nodelist from tradenetwork
  nodeList1 <- cbind(nodeList1, nodeDegree1=igraph::degree(tradenetwork1 , v = igraph::V(tradenetwork1), mode = "all")) #calculate node degree for all nodes in network

  
  # calculate the frequency that a node with a certain degree is occuring, for each node
  # we could make this directed because more imports might be related to more imports... if we wanted to 
  reshape <- dcast(nodeList1, Name ~ nodeDegree1, value.var = 'nodeDegree1')
  o <- unique(nodeList1$nodeDegree1)
  d=sapply(unique(nodeList1$nodeDegree1),function(x){length(which(nodeList1$nodeDegree1==x))})
  a = data.frame (o, d)
  nodeList1$frequency <- sapply(nodeList1$nodeDegree1, function(x){a$d[which(x==a$o)]})
  
nodeList1 = nodeList1 %>% rename("country" = "Name") 

names(nodeList1) <- c("country", "nodeDegree", "frequency")

#### for imports preferential attachment 
anchoancho = nl %>% filter(commodity == "Anchovies fresh or chilled") %>% 
  left_join(nodeList1, by = "country")%>% filter(!is.na(nodeDegree1) & new.imports >0)

write.csv(nodeList1, file = "nodeList1.csv")
  #( # new links added to nodes of degree n / total # of new links of new links in the network )
  
  ## number is the sum of 
  
  number = sum(anchoancho$new.imports)
  anchoancho$pk = (anchoancho$new.imports / number)  
  anchoancho$rk = anchoancho$pk / (anchoancho$frequency / length(nodeList1)) 
  
  require(ggplot2)
  rkplot = ggplot(anchoancho, aes(x = nodeDegree1, y = rk)) + geom_point() + scale_y_continuous(trans='log')
  rkplot = rkplot + labs(title="test preferential attachment", y = "relative probability", x = "nodedegree")
  
  
  #seems that there is preferential attachement for imports but not exports!
  summary(lm(anchoancho$new.imports ~ anchoancho$nodeDegree1))
  summary(lm(anchoancho$new.exports ~ anchoancho$nodeDegree1))
  
  summary(lm(anchoancho$rk ~ anchoancho$nodeDegree1)) #quite a bit clearer with the rk metric
  
  
  #link to stockstatus data and see if there is a relationship to stock status
  anchoancho$iso3 = anchoancho$country
   ancho.stock = data1 %>% filter(Commodity == "Anchovies fresh or chilled" & t == 1995) %>% 
    left_join(anchoancho, by = "iso3")

   summary(lm(cmsy13 ~ new.exports, data= ancho.stock))
   
   anchoancho2 = nl %>% filter(commodity == "Anchovies fresh or chilled") %>% 
     left_join(nodeList1, by = "country")%>% filter(!is.na(nodeDegree1) & new.exports >0)
   
   #####for exports preferential attachement
   #( # new links added to nodes of degree n / total # of new links of new links in the network )
   
   ## number is the sum of 
   
   number = sum(anchoancho2$new.exports)
   
   anchoancho2$pk = (anchoancho2$new.exports / number)  
   anchoancho2$rk = anchoancho2$pk / (anchoancho2$frequency / length(nodeList1))
   anchoancho2$iso3 = anchoancho2$country
   
   ancho.stock2 = data1 %>% filter(Commodity == "Anchovies fresh or chilled" & t == 1995) %>% 
     left_join(anchoancho2, by = "iso3")
   
   #small negative correlation between stock status and the relative chance of forming a new connection
   #but this is nonsence as we need to do something with the frequency 
   summary(lm(cmsy13 ~ rk, data= ancho.stock2))
   


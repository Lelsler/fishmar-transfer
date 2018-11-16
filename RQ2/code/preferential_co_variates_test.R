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
Rawdata.1998 <- read.csv("/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/processed/timeseries/comtrade_1992_year1998_with_stocks.csv")

country_codes = read.csv("/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/processed/timeseries/country_code_baci92.csv")

#is it the same?
colnames(country_codes) = c("imp.country", "imp.iso2", "imp.iso3", "j")

data1 = bind_rows(Rawdata.1995, Rawdata.1996, Rawdata.1997, Rawdata.1998) #gives warnings that don't matter
data1 = data1%>% left_join(country_codes,  by = "j")

#logfile with in and out trades
logfile = "new_links.csv"


#Columns
cat("country, year, new imports, new exports, commodity", file=logfile, append=FALSE, sep = "\n")



commodities = unique(data1$Commodity)

for (z in commodities){

data= subset(data1, Commodity == z)
#data = rawdata %>% filter(t == 1995 | t == 1996)  # and other years that we add.. 

#what data we need
CountryChanges <- data.frame(country=character(),
                             year=character(), 
                             import=integer(),
                             export=integer(),
                             stringsAsFactors=FALSE) 
colnames(CountryChanges) <- c("Country", "Year", "NewImports", "NewExports") # Because the column names didn't work right.
#fish species code

#I'm sure this could be done in a neater way but code below takes the iso codes 


exp = data.frame(unique(data$iso3))
imp = data.frame(as.factor(unique(data$imp.iso3)))


colnames(exp) = "country"
colnames(imp) = "country"

country = rbind(exp, imp)


years <- unique(data$t) # pull the unique set of years from the input file
countries <-unique(country$country)# unique countries in dataset
countries = as.character(countries)
#data$iso3 = as.character(data$iso3)
#data$j = as.character(data$j)

x <- length(years)
x <- x-1 # this said x- 2 but think it should be minus 1.. so changed it 
length(years) <- x # all years but the last year

for (k in years) { 
  
  # create appropriate first table, and first edgelist
  network.table1 <- data[data$t == k, ]
  edgeList1 <- data.frame(network.table1$iso3, network.table1$imp.iso3)
  colnames(edgeList1) = c("Exporter","Importer")

  #construct first network
  tradenetwork1 <- igraph::simplify(igraph::graph.data.frame(edgeList1, directed=TRUE))
  nodeList1 <- data.frame(Name = igraph::V(tradenetwork1)$name) # create nodelist from tradenetwork
  nodeList1 <- cbind(nodeList1, nodeDegree1=igraph::degree(tradenetwork1 , v = igraph::V(tradenetwork1), mode = "all")) #calculate node degree for all nodes in network
  
  nextYear <- k + 1
  # create appropriate second table, and first edgelist
  network.table2 <- data[data$t == nextYear, ]
  edgeList2 <- data.frame(network.table2$iso3, network.table2$imp.iso3) #this needs to be changed into importer and exporter isocodes
  colnames(edgeList2) = c("Exporter","Importer")
  
  
  edgeList2$Exporter = as.character(edgeList2$Exporter)
  edgeList1$Exporter = as.character(edgeList1$Exporter)
  edgeList2$Importer == as.character(edgeList2$Importer)
  edgeList1$Importer == as.character(edgeList1$Importer)
  
  #construct second network
  tradenetwork2 <- igraph::simplify(igraph::graph.data.frame(edgeList2, directed=TRUE))
  nodeList2 <- data.frame(Name = igraph::V(tradenetwork2)$name) # create nodelist from tradenetwork
  nodeList2 <- cbind(nodeList2, nodeDegree2=igraph::degree(tradenetwork2 , v = igraph::V(tradenetwork2), mode = "all")) #calculate node degree for all nodes in network
  
  ###### New Links
  for (m in countries) {
    
    c2 <- edgeList2[edgeList2$Exporter == m, ]
    c1 <- edgeList1[edgeList1$Exporter == m, ]
    
    comp <- c2$Importer %in% c1$Importer # New Imports
    ImpOld <- sum(comp, na.rm = TRUE) # Count the Trues
    Imp <- length(comp)-ImpOld # count the false (which are the new countries)
    
    
    c2 <- edgeList2[edgeList2$Importer == m, ]
    c1 <- edgeList1[edgeList1$Importer == m, ]
    
    comp <- c2$Exporter %in% c1$Exporter # New Exports
    ExpOld <- sum(comp, na.rm = TRUE) # Count the Trues
    Exp <- length(comp)-ExpOld # count the false (which are the new countries)
    
    NewRow <- c(m, k+1, Imp, Exp, z)
    
    #colnames(NewRow) = c("country", "year", "new imports", "new exports", "commodity")
    cat(NewRow, file=logfile, append=TRUE, sep = ",")#save in logfile
    cat("\n",file=logfile, append=TRUE)
    
   # CountryChanges <- rbind(CountryChanges, NewRow)
    
    print(NewRow)
    
  }
  
}
  
}

#all new links
nl = read.csv("new_links.csv") #this works only if commas in orignial file are removed

###### End 




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
  
nodeList1 = nodeList1 %>% rename(country = Name) 

####for imports
anchoancho = nl %>% filter(commodity == "Anchovies fresh or chilled") %>% 
  left_join(nodeList1, by = "country")%>% filter(!is.na(nodeDegree1) & new.imports >0)

  
  #( # new links added to nodes of degree n / total # of new links of new links in the network )
  
  ## number is the sum of 
  
  number = sum(anchoancho$new.imports)
  
  anchoancho$pk = (anchoancho$new.imports / number)  
  
  anchoancho$rk = anchoancho$pk / (anchoancho$frequency / length(nodeList1)) 
  
  
  
  #require(ggplot2)
  rkplot = ggplot(anchoancho, aes(x = nodeDegree1, y = rk)) + geom_point() + scale_y_continuous(trans='log')
  rkplot = rkplot + labs(title="test preferential attachment", y = "relative probability", x = "nodedegree")
  
  
  #seems that there is preferential attachement for imports but not exports!
  summary(lm(anchoancho$new.imports ~ anchoancho$nodeDegree1))
  summary(lm(anchoancho$new.exports ~ anchoancho$nodeDegree1))
  
  summary(lm(anchoancho$rk ~ anchoancho$nodeDegree1)) #quite a bit clearer with the rk metric
  
  anchoancho$iso3 = anchoancho$country
  
   ancho.stock = data1 %>% filter(Commodity == "Anchovies fresh or chilled" & t == 1995) %>% 
    left_join(anchoancho, by = "iso3")

   summary(lm(cmsy13 ~ new.exports, data= ancho.stock))
   
   
   
   anchoancho2 = nl %>% filter(commodity == "Anchovies fresh or chilled") %>% 
     left_join(nodeList1, by = "country")%>% filter(!is.na(nodeDegree1) & new.exports >0)
   
   #####for exports
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
   


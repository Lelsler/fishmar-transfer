# Clear workspace
rm(list = ls())
graphics.off() 

# for now this code is only looking at net increases and decreases in node-degree, while we are actually interested 
# in all the new connections that are formed. 

# libraries
require(igraph)
require(dplyr)
require(reshape2)

setwd("/Users/mtn1/Nextcloud/FISHMAR-data/rq2/test_preferential/")

data2 = read.csv("HS92_comtrade_trades.csv")
data2 = data2[, c(2:9)]
#logfile with in and out trades
#logfile = "new_links_bilateral_imports.csv"

#Columns
#cat("importing_country, exporting_country, year, amount,  commodity", file=logfile, #append=FALSE, sep = "\n")

commodities = unique(data2$Shortdescription.HS1992)

#what data we need
newlinks <- data.frame(year = integer(),
hs6=character(),     
commodity=character(),
commodity_code=character(),
iso3=character(), 
imp.iso3=character(),
export.value = numeric(),
export.quantity = numeric(),
new.export = character(),
stringsAsFactors=FALSE) 

colnames(newlinks) <- c("t", "hs6","Shortdescription.HS1992", "Code.HS1992", "iso3", "imp.iso3", "v", "q", "new.export") # Because the column names didn't work right.

#fish species code
for (z in commodities){
  data= subset(data2, commodities == z)
  
  #I'm sure this could be done in a neater way but code below takes the iso codes 
  exp = data.frame(unique(data$iso3))
  imp = data.frame(as.factor(unique(data$imp.iso3)))
  
  colnames(exp) = "country"
  colnames(imp) = "country"
  
  country = rbind(exp, imp)
  
  years <- unique(data$t) # pull the unique set of years from the input file
  countries <-unique(country$country)# unique countries in dataset
  countries = as.character(countries)
  
  x <- length(years)
  x <- x-1 # this said x- 2 but think it should be minus 1.. so changed it 
  length(years) <- x # all years but the last year
  
  for (k in years) { 
    
    table1 <- data[data$t == k, ]
    #edgeList1 <- table1 %>% select(iso3, imp.iso3)
    
    nextYear <- k + 1
    # create appropriate second table, and first edgelist
    table2 <- data[data$t == nextYear, ]
    
    ###### New Links
    for (m in countries) {
      
      
      c2 <- table2[table2$iso3 == m, ]
      c1 <- table1[table1$iso3 == m, ]
      
      comp <- c2$imp.iso3 %in% c1$imp.iso3 # New Imports
      
      c2$new.export = ifelse((c2$imp.iso3 %in% c1$imp.iso3), "old_connection", "new_connection")
      
      newlinks = bind_rows(c2, newlinks)
      print(newlinks)
      }
  }
}

write.csv(newlinks, "newlinksprelim.csv")


# Clear workspace
rm(list = ls())
graphics.off() 

# for now this code is only looking at net increases and decreases in node-degree, while we are actually interested 
# in all the new connections that are formed. 

# libraries
#require(igraph)
require(dplyr)
require(reshape2)

setwd("/Users/mtn1/Nextcloud/FISHMAR-data/rq2/test_preferential/")
load("timeseries_complete.RData")

country_codes = read.csv("/Users/mtn1/Nextcloud/FISHMAR-data/comtrade/processed/timeseries/country_code_baci92.csv")

#is it the same?
colnames(country_codes) = c("imp.country", "imp.iso2", "imp.iso3", "j")
data = data2%>% left_join(country_codes,  by = "j")

#data = data %>% select(t, hs6, Shortdescription.HS1992, Code.HS1992, iso3, imp.iso3, v, q) 

data = as_tibble(data)
data = distinct(data)

data = data %>% filter(Shortdescription.HS1992!= "Fish nes, fresh or chilled, whole" &
                          Shortdescription.HS1992!="Fish nes, frozen, whole" & 
                          Shortdescription.HS1992!="Fish fillets, frozen" & 
                          Shortdescription.HS1992!="Dried fish, other than cod, not smoked"&
                          Shortdescription.HS1992!= "Fish fillet or meat, fresh or chilled, not liver, roe" &
                          Shortdescription.HS1992!="Fish meat & mince, except liver, roe & fillets, froze" &
                          Shortdescription.HS1992!="Flour or meal, pellet, fish, etc, for animal feed"&
                          Shortdescription.HS1992!="Fish prepared or preserved, except whole, in pieces"&
                          Shortdescription.HS1992!="Fish nes, salted or in brine, not dried or smoked" &
                          Shortdescription.HS1992!="Fish livers and roes, frozen" &
                          Shortdescription.HS1992!="Fish fillets, dried, salted or in brine, not smoked" &
                          Shortdescription.HS1992!="Smoked fish & fillets other than herrings or salmon"&
                          Shortdescription.HS1992!= "Fish-liver oils, fractions, not chemically modified"&
                          Shortdescription.HS1992!= "Fish oils except liver, not chemically modified" &
                          Shortdescription.HS1992!= "Fish nes, prepared or preserved, not minced" &
                          Shortdescription.HS1992!= "Livers and roes, dried, smoked, salted or in brine" &
                          Shortdescription.HS1992!= "Sauces nes, mixed condiments, mixed seasoning" &
                          Shortdescription.HS1992!= "Seaweeds and other algae" 
                            )

data = data %>% filter(Shortdescription.HS1992!="Seaweeds and other algae," & Shortdescription.HS1992!="Fish live, except trout, eel or carp" &
                         Shortdescription.HS1992!="Coral,seashell,cuttle bone,etc, unworked,powder,waste")


save(data, file= "timeseries_complete.RData")
write.csv(data, "timeseries_complete.csv")



#what data we need
commodity_groups <- data.frame(matched.commodities = character(),
                               original.commodity=character(),
                             stringsAsFactors=FALSE)

shorts = unique(data$Shortdescription.HS1992)

for (m in shorts) {

shrimp = data[data$Shortdescription.HS1992==m, ]
nofrozensh = data[data$Shortdescription.HS1992!=m, ]

match = nofrozensh$Shortdescription.HS1992[!is.na(match(nofrozensh$sci_name, shrimp$sci_name))]
list1 = unique(match)
list2 = unique(shrimp$Shortdescription.HS1992)

merger = merge(list1, list2)
colnames(merger) = c("matched.commodities", "original.commodity")

commodity_groups = rbind(commodity_groups, merger)

}

write.csv(commodity_groups, "commodity_groups.csv")

#
#logfile with in and out trades
logfile = "new_links_bilateral_imports.csv"

#Columns
cat("importing_country, exporting_country, year, amount,  commodity", file=logfile, append=FALSE, sep = "\n")

commodities = unique(data2$Shortdescription.HS1992)

#what data we need
CountryChanges <- data.frame(commodity = character(),
                             exporting.country=character(),
                             importing.country=character(),
                             year=character(), 
                             export.volume=numeric(),
                             export.value = numeric(),
                             stringsAsFactors=FALSE) 

colnames(CountryChanges) <- c("commodity", "exporting.country","importing.country", "Year", "NewImports", "NewExports") # Because the column names didn't work right.


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
      
      c2$new.import = ifelse((c2$imp.iso3 %in% c1$imp.iso3), "old_connection", "new_connection")
      
      #colnames(NewRow) = c("country", "year", "new imports", "new exports", "commodity")
      #cat(NewRow, file=logfile, append=TRUE, sep = ",")#save in logfile
      #cat("\n",file=logfile, append=TRUE)
      
      # CountryChanges <- rbind(CountryChanges, NewRow)
      
      print(c2)
      
    }
    
  }
  
}
###### End 
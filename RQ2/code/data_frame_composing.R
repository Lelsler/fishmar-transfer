#___________________________________________________________________________________________________________#
# Matching countries for governance index & FAO trade data
#___________________________________________________________________________________________________________#

require(dplyr)
#remove lists
rm(list = ls())

#set working directory
setwd("/Users/mtn1/Desktop/R_projects/fishmar/data")

#load governance effectiveness data from worldbank. The indicator is an estimate, I'm not considering the standard deviations and confidence intervals that are in the original data
gov_eff = read.csv("/Users/mtn1/Desktop/R_projects/fishmar/data/World_bank_data/governance_effective.csv") 
#load stock assessment data based on the landings reported to FAO
stock = read.csv("/Users/mtn1/Desktop/R_projects/fishmar/data/fao_stock_status/processed/1950_2017_FAO_bbmsy_timeseries_merge.csv")
#load Comtrade data
trade = read.csv("/Users/mtn1/Desktop/R_projects/fishmar/data/com_trade/CT_max_toothfish_29Jan18.csv") 

# load geographical distance data 
dist = read.csv("/Users/mtn1/Desktop/R_projects/fishmar/RQ2/data/dist_cepii.csv") 

dist$Importer.ISO = dist$iso_o
dist$Exporter.ISO = dist$iso_d



#change the silly columnnames of the datafile
colnames(gov_eff) = c("country","iso3", "1996", "1998", "2000", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")

#reduce the datafile to what's needed
gov_eff = gov_eff[, 2:20]
rownames(gov_eff) = gov_eff$iso3

#flip the dataframe so that each gov. eff. indicator is in a separate row
df = data.frame(ISO = row.names(gov_eff), stack(gov_eff))
colnames(df) = c("iso3", "geff", "year") #change colnames to the like colnames in the landings data

#this could be done with one dataframe I'm quite sure, maybe Chris knows how :)
df_CT = df
colnames(df_CT) = c("Importer.ISO", "geff.Importer", "Year")

df_CT2 = df
colnames(df_CT2) = c("Exporter.ISO", "geff.Exporter", "Year")

#then merge both of them, one for the importer and one for the exported governance effectiveness
CT_trade = merge(trade, df_CT, all=TRUE)
CT_trade = merge(CT_trade, df_CT2, all=TRUE)

#filter the stock estimates for patagonian toothfish
data = stock %>% filter(comm_name == "Patagonian toothfish")
data$Year = data$year

#take out a very general commodity class of the patagonian toothfish, but check later if it's needed
CT_trade = CT_trade %>% filter(Commodity.Code != "30429")


# merge the dataframes based on exporter iso
data$Exporter.ISO = data$iso3
tryout = merge(data, CT_trade)

#merge distance data & stock estimate & trade data

tryout = merge(tryout, dist)

#calculate mean and standard deviations of importer and exporter governance
mean(na.omit(tryout$geff.Importer))
mean(na.omit(tryout$geff.Exporter))

sd(na.omit(tryout$geff.Importer))
sd(na.omit(tryout$geff.Exporter))

#use dis cap which is the distance between capitals 

l = lm(tryout$geff.Importer ~ tryout$Year)
summary(l)

#plots with exporter governance changes over time
boxplot(tryout$geff.Exporter ~ tryout$Year)

#boxplot(tryout$dist ~ tryout$Year)

# construct network from trade data

#construct network

edgelist = tryout[, 1:2]

tradenetwork = igraph::simplify(igraph::graph.data.frame(edgelist, directed=TRUE))
nodeList = data.frame(Name = igraph::V(tradenetwork)$name, 
                       ID = c(0:(igraph::vcount(tradenetwork) - 1))) # create nodelist from tradenetwork
nodeList <- cbind(nodeList, nodeDegree=igraph::degree(tradenetwork, v = igraph::V(tradenetwork), mode = "all")) #calculate node degree for all nodes in network


plot(tradenetwork, vertex.color="pink", edge.color ="dark grey",
     vertex.size=nodeList$nodeDegree,
     edge.arrow.size=.1, vertex.label.color="black", vertex.label.cex=.2, rescale=FALSE)

plot(tradenetwork,  vertex.label.color="black", vertex.color="pink", vertex.size=nodeList$nodeDegree, edge.arrow.size=.1, ertex.label.color="black", vertex.label.cex=.7)

tryout1 = tryout[tryout$Exporter.ISO=="ARG", ]
plot(tryout1$Year, tryout1$super, xlab="Year", ylab= "Stock status ARG", pch=20, ylim=c(0,1.2), col="blue", main="Stock status ARG")

tryout2 = tryout[tryout$Exporter.ISO=="CHL", ]
plot(tryout2$Year, tryout2$super, xlab="Year", ylab= "Stock status CHL", pch=20, ylim=c(0,1.2), col="blue", main="Stock status CHL")

tryout3 = tryout[tryout$Exporter.ISO=="FLK", ]
plot(tryout3$Year, tryout3$super, xlab="Year", ylab= "Stock status FLK", pch=20, ylim=c(0,1.2), col="blue", main="Stock status CHL")



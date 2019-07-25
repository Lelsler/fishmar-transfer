# Clear workspace
rm(list = ls())
graphics.off() 

# libraries
library(readr)
#library(dplyr)
library(tidyverse)
library(plotly)
library(TTR)

setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/")

# read files
stock_status <- read.csv("./mexico/processed/data_RQ3_filtered.csv")
monthly_effort <- read.csv("./mexico/processed/laura/RQ3_monthly_dataset_lge.csv")
catchability <- read.csv("./mexico/processed/laura/coop_catchability_coeffecient.csv")
bio <- read.csv("./mexico/processed/biological_data_RQ3.csv")
fmsy <- read.csv("./rq3/stock_assessments/results_MSY_rq3.csv")

# clean names 
setnames(stock_status, old=c("X...Coop_Id","Cooperative.name","Abalone","Clams","Lobsters","Sea.cucumber","Snails"), new=c("coop_id", "coop_name","stock_abalone","stock_clams","stock_lobsters","stock_seacucumber","stock_snails"))

# select columns
monthly_effort_select <- monthly_effort %>% select(ID.x, Coop_Id, Cooperative_name, State.x,Confederacion.x,Federacion.x,functionality.x,year,
                          month,boats_abalone,boats_clams,boats_lobsters,
                          boats_seacucumber,boats_snails,catch_abalone,catch_clams,
                          catch_lobsters,catch_seacucumber,catch_snails,days_abalone,
                          days_clams,days_lobsters,days_seacucumber,days_snails,
                          value_abalone,value_clams,value_lobsters,value_seacucumber,
                          value_snails)

# set names 
setnames(monthly_effort_select, old=c("ID.x","Coop_Id","Cooperative_name","State.x","Confederacion.x","Federacion.x","functionality.x"), new=c("ID","coop_id", "coop_name","state","confederacion","federacion","functionality"))

# merge data sets 
stock_monthly <- left_join(monthly_effort_select,stock_status, by=c("coop_id","coop_name","year")) 

# calc new columns for reference price M for each species
stock_monthly$m_abalone = (stock_monthly$value_abalone/log(stock_monthly$catch_abalone))
stock_monthly$m_clams = (stock_monthly$value_clams/log(stock_monthly$catch_clams))
stock_monthly$m_lobsters = (stock_monthly$value_lobsters/log(stock_monthly$catch_lobsters))
stock_monthly$m_seacucumber = (stock_monthly$value_seacucumber/log(stock_monthly$value_seacucumber))
stock_monthly$m_snails = (stock_monthly$value_snails/log(stock_monthly$catch_snails))

# calc new columns for harvest rate h for each species
stock_monthly$h_abalone = stock_monthly$catch_abalone/stock_monthly$stock_abalone
stock_monthly$h_clams = stock_monthly$catch_clams/stock_monthly$stock_clams
stock_monthly$h_lobsters = stock_monthly$catch_lobsters/stock_monthly$stock_lobsters
stock_monthly$h_seacucumber = stock_monthly$catch_seacucumber/stock_monthly$stock_seacucumber
stock_monthly$h_snails = stock_monthly$catch_snails/stock_monthly$stock_snails

# add catchability coefficients for each cooperative (I first did this after reading the csv file in case there is an error)
stock_monthly= left_join(x=stock_monthly, y = catchability, by= "coop_id",  all.x=TRUE) # stock_monthly, catchability

### add bmsy
# create individual files
colnames(bio)[colnames(bio)=="Subregion"] <- "coop_name" 
ab <- bio[(bio$Group=="Abalone"),]
cl <- bio[(bio$Group=="Clams"),]
lo <- bio[(bio$Group=="Lobsters"),]
se <- bio[(bio$Group=="Sea cucumber"),]
sn <- bio[(bio$Group=="Snails"),]

# change column names 
colnames(ab)[colnames(ab)=="Bmsy"] <- "Bmsy_abalone" 
colnames(cl)[colnames(cl)=="Bmsy"] <- "Bmsy_clams" 
colnames(lo)[colnames(lo)=="Bmsy"] <- "Bmsy_lobsters" 
colnames(se)[colnames(se)=="Bmsy"] <- "Bmsy_seacucumber" 
colnames(sn)[colnames(sn)=="Bmsy"] <- "Bmsy_snails" 

# merge sub-files (bio)
ab <- merge(ab[ ,c("coop_name","Bmsy_abalone")],cl[ ,c("coop_name","Bmsy_clams")],by="coop_name",all=T)
ab <- merge(ab,lo[ ,c("coop_name","Bmsy_lobsters")],by="coop_name",all=T)
ab <- merge(ab,se[ ,c("coop_name","Bmsy_seacucumber")],by="coop_name",all=T)
ab <- merge(ab,sn[ ,c("coop_name","Bmsy_snails")],by="coop_name",all=T)

# add to main data
stock_monthly <- merge(stock_monthly,ab,by="coop_name",all=T)

### add fmsy
# create individual files
colnames(fmsy)[colnames(fmsy)=="Subregion"] <- "coop_name" 
ab <- fmsy[(fmsy$Group=="Abalone"),]
cl <- fmsy[(fmsy$Group=="Clams"),]
lo <- fmsy[(fmsy$Group=="Lobsters"),]
se <- fmsy[(fmsy$Group=="Sea cucumber"),]
sn <- fmsy[(fmsy$Group=="Snails"),]

# change column names 
colnames(ab)[colnames(ab)=="F_msy"] <- "Fmsy_abalone" 
colnames(cl)[colnames(cl)=="F_msy"] <- "Fmsy_clams" 
colnames(lo)[colnames(lo)=="F_msy"] <- "Fmsy_lobsters" 
colnames(se)[colnames(se)=="F_msy"] <- "Fmsy_seacucumber" 
colnames(sn)[colnames(sn)=="F_msy"] <- "Fmsy_snails" 

# merge sub-files (bio)
ab <- merge(ab[ ,c("coop_name","Fmsy_abalone")],cl[ ,c("coop_name","Fmsy_clams")],by="coop_name",all=T)
ab <- merge(ab,lo[ ,c("coop_name","Fmsy_lobsters")],by="coop_name",all=T)
ab <- merge(ab,se[ ,c("coop_name","Fmsy_seacucumber")],by="coop_name",all=T)
ab <- merge(ab,sn[ ,c("coop_name","Fmsy_snails")],by="coop_name",all=T)

# add to main data
stock_monthly <- merge(stock_monthly,ab,by="coop_name",all=T)

### save csv file
#write.csv(stock_monthly, './mexico/processed/laura/data_monthly_lge.csv',row.names=FALSE)

################################# for plotting #########################################################
# read csv
stock_monthly <- read.csv("./mexico/processed/laura/data_monthly_lge.csv")

plot(stock_monthly$m_abalone,stock_monthly$h_abalone)
plot(stock_monthly$m_clams,stock_monthly$h_clams)
plot(stock_monthly$m_lobsters,stock_monthly$h_lobsters)
plot(stock_monthly$m_seacucumber,stock_monthly$h_seacucumber)
plot(stock_monthly$m_snails,stock_monthly$h_snails)

################################# calculations ##########################################################



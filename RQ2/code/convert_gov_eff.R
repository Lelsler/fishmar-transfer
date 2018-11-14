#clear workspace
rm(list = ls())
graphics.off()

setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2")

require(dplyr)
library(tidyr)

#load required data
goveff = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/governance/governance_effective_processed.csv") # governance effectiveness indicators

# delete X in front of the column names
colnames(goveff) <- gsub("^X", "",  colnames(goveff))

# point R to column names in df
colnames(goveff) = c("country", "iso3", "1996", "1997", 1998:2016)

# re-format gov
gov = goveff %>%
  gather(t, gov_eff, -country, -iso3)


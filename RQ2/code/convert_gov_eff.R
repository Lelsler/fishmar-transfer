#clear workspace
rm(list = ls())
graphics.off()

setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2")

require(dplyr)
library(tidyr)

#load required data
gov_d = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/governance/governance_effective_processed.csv") # governance effectiveness indicators

library(tibble)
gov_d <- as_tibble(read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/co_variates_data/governance/governance_effective_processed.csv", stringsAsFactors = FALSE, check.names = FALSE))

gather(gov_d, year, effe, -1)

gov_d %>%
  gather(year, effe, 2:24)


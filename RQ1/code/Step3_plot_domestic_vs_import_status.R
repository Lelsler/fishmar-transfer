
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "RQ1/data"
govdir <- "~/Nextcloud/FISHMAR-data/wb_indicators"


# Read data
data_orig <- read.csv(file.path(datadir, "domestic_vs_import_stock_status.csv"), as.is=T)
gov_orig <- read.csv(file.path(govdir, "world_bank_composite_governance_index.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce to countries w/ more than 5 domestic stocks and with import stocks
  filter(!is.na(imp_bbmsy) & nstocks>=5) %>%
  arrange(bbmsy) %>%
  # Add goverance
  left_join(select(gov_orig, iso3, gov_index, gov_strength), by="iso3")
  


# Plot data
################################################################################

# 
plot(bbmsy ~ gov_index, data)


# Plot domestic status
plot(x=data$bbmsy, y=1:nrow(data), bty="n", xlim=c(0,2), 
     yaxt="n", ylab="", xlab=expression("B/B"["MSY"]),
     pch=16)
points(x=data$imp_bbmsy, y=1:nrow(data), pch=12, col="red")
abline(v=1, lty=3)

hist(data$imp_nstocks, breaks=seq(0,1000,20))







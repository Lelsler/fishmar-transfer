
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)


# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
tradedir <- "~/Nextcloud/FISHMAR-data/comtrade/processed/timeseries"
statusdir <- "~/Nextcloud/FISHMAR-data/fao_stock_status"

# Read data
trade_orig <- read.csv(file.path(tradedir, "CT_fish_trade92.csv"), as.is=T)
key_orig <- read.csv(file.path(tradedir, "matchingstocksHS92.csv"), as.is=T)
status_orig <- read.csv(file.path(statusdir, "1950_2017_FAO_bbmsy_timeseries_merge.csv"), as.is=T)


# Format data
################################################################################

# Format trade data
# Quantity is allegedly in 
trade <- trade_orig %>%
  # Rename columns
  rename(year=t, hs92_desc=Shortdescription.HS1992, hs92=Code.HS1992,
         exp_iso3=iso3, imp_iso3=imp.iso3, value_usd=v, quantity_kg=q, catg_ours=group_name) %>% 
  # Rearrange columns
  select(imp_iso3, exp_iso3, hs92, hs92_desc, catg_ours, year, value_usd, quantity_kg, everything()) %>% 
  # Sort
  arrange(imp_iso3, exp_iso3, hs92, hs92_desc, catg_ours, year)

# Format commodity key
# There are some HS codes with common name but not scientific name (FIX THIS)
# There are some HS codes without 
key <- key_orig %>% 
  # Remove extra columns
  select(-c(X, X.1)) %>% 
  # Rename columns
  rename(hs92_desc=Shortdescription.HS1992) %>% 
  # Filter some problems
  filter(!is.na(hs92_desc) & !is.na(comm_name))

# Create catch weighted time series
status <- status_orig %>% 
  # Add mean catch by FAO-country-species stock
  group_by(stockid) %>% 
  mutate(tl_mt_avg=mean(tl_mt, na.rm=T)) %>% 
  # Calculate catch-weighted B/BMSY by country-species stock
  group_by(iso3, country, sci_name, comm_name, year) %>% 
  summarize(bbmsy_avg=weighted.mean(x=tl_mt, w=tl_mt_avg))


# Figure out workflow using US data
################################################################################

# Categories to exclude
groups_exclude <- c("shrimp", "salmon")

# US import time series
us_imports <- trade %>% 
  filter(imp_iso3=="USA")

# Major US importing countries
us_import_countries <- trade %>% 
  filter(imp_iso3=="USA") %>% 
  group_by(exp_iso3) %>% 
  summarize(total_kg=sum(quantity_kg, na.rm=T)) %>% 
  arrange(desc(total_kg))

# Identify the stocks contributing to US imports
us_import_stocks <- trade %>% 
  filter(imp_iso3=="USA" & year %in% 2012:2016 & !catg_ours%in%groups_exclude) %>% 
  group_by(exp_iso3, exp_iso3, catg_ours) %>% 
  summarize(hs92_code=paste(sort(unique(hs92)), collapse=", "),
            q_avg_kg=mean(quantity_kg, na.rm=T),
            v_avg_usd=mean(value_usd, na.rm=T))

# Identify status time series of stocks contributing to US imports
us_import_stocks_ts







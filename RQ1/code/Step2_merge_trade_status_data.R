
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)


# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
tradedir <- "~/Nextcloud/FISHMAR-data/comtrade/raw/2012_comtrade"
statusdir <- "~/Nextcloud/FISHMAR-data/fao_stock_status"
govdir <- "~/Nextcloud/FISHMAR-data/wb_indicators"
outputdir <- "RQ1/data"

# Read data
trade_orig <- readRDS(file.path(tradedir, "2012-16_comtrade_hs12_data_use.Rds"))
key_orig <- read.csv(file.path(tradedir, "hs6_fao_status_code_key.csv"), as.is=T)
status_orig <- read.csv(file.path(statusdir, "1950_2017_FAO_bbmsy_timeseries_merge.csv"), as.is=T)
gov_orig <- read.csv(file.path(govdir, "world_bank_composite_governance_index.csv"), as.is=T)


# Format data
################################################################################

# Format commodity key
# There are still missing common/scientific names
key <- key_orig %>% 
  # Rename columns
  rename(hs12code=Commodity.HS2012code, hs12desc=Commodity.HS2012, fao_desc=Commodity,
         comm_name=Comm.Name, sci_name=Sci.Name.Orig, use=Usage)

# Build simple HS key linking HS code and HS description
# Both the HS codes and HS descriptions should be unique
hs_key <- key %>%
  select(hs12code, hs12desc) %>% 
  filter(hs12desc!="") %>% 
  unique()
anyDuplicated(hs_key$hs12code)
anyDuplicated(hs_key$hs12desc)

# Create key for used commodities only
key_use <- key %>% 
  select(hs12code, hs12desc, comm_name, use) %>% 
  filter(comm_name!="" & use=="2_used")

# Format trade data
trade <- trade_orig %>% 
  left_join(select(hs_key, hs12code, hs12desc), by="hs12code") %>% 
  select(imp_iso3, exp_iso3, hs12code, hs12desc, year, value_usd, quantity_mt) %>% 
  arrange(imp_iso3, exp_iso3, hs12code, hs12desc, year)

# Create catch weighted time series
status <- status_orig %>% 
  # Add mean catch by FAO-country-species stock
  group_by(stockid) %>% 
  mutate(tl_mt_avg=mean(tl_mt, na.rm=T)) %>% 
  # Calculate catch-weighted B/BMSY by country-species stock
  group_by(iso3, country, sci_name, comm_name, year) %>% 
  summarize(bbmsy_avg=weighted.mean(x=super, w=tl_mt_avg),
            tl_mt_avg=sum(tl_mt_avg)) %>% 
  # Add stock id and rearrange
  mutate(stockid=paste(iso3, comm_name, sep="-")) %>% 
  select(stockid, everything())

# Build status key (to show all possible stocks and terminal B/BMSY)
status_key <- filter(status, year==2015)


# Step 1. Calculate mean domestic status
################################################################################

# Domestic status
# Mean (unweighted) terminal year status
# Salmon and shrimp excluded
range(status$year)
status_home <- status %>% 
  filter(year==2015 & !grepl("shrimp", comm_name) & !grepl("salmon", comm_name)) %>% 
  group_by(iso3, country) %>% 
  summarize(nstocks=n(),
            bbmsy=mean(bbmsy_avg),
            bbmsy_sd=sd(bbmsy_avg))


# Step 2. Calculate mean import status
################################################################################

# ISO3s to evaluate
iso3s <- sort(unique(trade$imp_iso3))

# Function to calculate mean (unweighted) import stock status
iso3 <- "USA"
mean_import_status <- function(iso3){
  
  # Import time series for target country
  import_ts <- trade %>% 
    filter(imp_iso3==iso3)
  
  # Countries providing imports to the target country
  import_countries <- trade %>% 
    filter(imp_iso3==iso3) %>% 
    group_by(exp_iso3) %>% 
    summarize(total_mt=sum(quantity_mt, na.rm=T)) %>% 
    arrange(desc(total_mt))
  
  # Stocks providing imports to the target country (last 5 years)
  # Many of these stocks may not actually have status estimates in the status database
  import_stocks <- trade %>% 
    filter(imp_iso3==iso3 & year %in% 2012:2016) %>% 
    group_by(exp_iso3, exp_iso3, hs12code, hs12desc) %>% 
    summarize(q_avg_mt=mean(quantity_mt, na.rm=T),
              v_avg_usd=mean(value_usd, na.rm=T)) %>% 
    # Add status common names associated with each commodity
    left_join(select(key, hs12code, comm_name), by="hs12code") %>% 
    # Remove stocks without match
    filter(comm_name!="") %>% 
    # Create stock id
    mutate(stockid=paste(exp_iso3, comm_name, sep="-")) %>%
    # Reduce to stocks that exist in status data
    filter(stockid %in% status_key$stockid) %>% 
    # Reduce to unique stocks and total trade
    group_by(stockid, exp_iso3, comm_name) %>% 
    summarize(quantity_mt=sum(q_avg_mt), 
              value_usd=sum(v_avg_usd)) %>% 
    ungroup()
  
  # Status time series for stocks providing imports to the target country
  import_stocks_ts <- status %>%
    filter(stockid %in% import_stocks$stockid)
  
  # Calculate terminal year statistics
  stats <- import_stocks_ts %>% 
    # Most recent year
    filter(year==2015) %>% 
    # Add ISO3 of target country (the importing country)
    rename(exp_iso3=iso3) %>% 
    mutate(iso3=iso3) %>% 
    # Add trade demand on each imported stock
    left_join(select(import_stocks, stockid, quantity_mt, value_usd), by="stockid") %>% 
    rename(import_mt=quantity_mt, import_usd=value_usd) %>% 
    # Summarize across imported stocks
    group_by(iso3) %>% 
    summarize(imp_nstocks=n(),
              imp_bbmsy=mean(bbmsy_avg),
              imp_bbmsy_sd=sd(bbmsy_avg),
              imp_bbmsy_mt=weighted.mean(bbmsy_avg, import_mt),
              imp_bbmsy_usd=weighted.mean(bbmsy_avg, import_usd)) %>% 
    ungroup()
  
  # Return
  return(stats)
  
}

# Loop through ISOs and calculate mean import status
status_imports <- purrr::map_df(iso3s, function(x) mean_import_status(x))


# Step 3. Merge domestic and import status
################################################################################

# Merge data, calculate ratio or percent difference
data <- status_home %>% 
  # Add import stock status
  left_join(status_imports, by="iso3")


# Export data
write.csv(data, file=file.path(outputdir, "domestic_vs_import_stock_status.csv"), row.names=F)  
  
  
  

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
govdir <- "~/Nextcloud/FISHMAR-data/wb_indicators"

# Read data
trade_orig <- read.csv(file.path(tradedir, "CT_fish_trade92.csv"), as.is=T)
key_orig <- read.csv(file.path(tradedir, "matchingstocksHS92.csv"), as.is=T)
status_orig <- read.csv(file.path(statusdir, "1950_2017_FAO_bbmsy_timeseries_merge.csv"), as.is=T)
gov_orig <- read.csv(file.path(govdir, "WB_rule_of_law_avg_by_iso3.csv"), as.is=T)

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
# There are some HS codes with common name but not scientific name (FIX THIS - often duplicates)
# There are some HS codes without 
key <- key_orig %>% 
  # Remove extra columns
  select(-c(X, X.1)) %>% 
  # Rename columns
  rename(hs92_desc=Shortdescription.HS1992) %>% 
  # Filter some problems
  filter(!is.na(hs92_desc) & !is.na(comm_name) & !is.na(sci_name))

# Make sure that the key presents unique rows
# The following two values must be equal
nrow(key)
n_distinct(paste(key$hs92_desc, key$comm_name))

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


# Figure out workflow using US data
################################################################################


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
iso3s <- sort(unique(trade$exp_iso3))

# Function to calculate mean (unweighted) import stock status
iso3 <- "USA"
mean_import_status <- function(iso3){

  # Categories to exclude
  groups_exclude <- c("shrimp", "salmon")
  
  # Import time series for target country
  import_ts <- trade %>% 
    filter(imp_iso3==iso3)
  
  # Countries providing imports to the target country
  import_countries <- trade %>% 
    filter(imp_iso3==iso3 & !catg_ours%in%groups_exclude) %>% 
    group_by(exp_iso3) %>% 
    summarize(total_kg=sum(quantity_kg, na.rm=T)) %>% 
    arrange(desc(total_kg))
  
  # Stocks providing imports to the target country (last 5 years)
  # Many of these stocks may not actually have status estimates in the status database
  import_stocks <- trade %>% 
    filter(imp_iso3==iso3 & year %in% 2012:2016 & !catg_ours%in%groups_exclude) %>% 
    group_by(exp_iso3, exp_iso3, hs92, hs92_desc) %>% 
    summarize(q_avg_kg=mean(quantity_kg, na.rm=T),
              v_avg_usd=mean(value_usd, na.rm=T)) %>% 
    # Add status common names associated with each commodity
    left_join(select(key, hs92_desc, comm_name), by="hs92_desc") %>% 
    # Create stock id
    mutate(stockid=paste(exp_iso3, comm_name, sep="-")) #%>%
    # Reduce to stocks that exist in status data
    # filter(stockid %in% status_key$stockid)
  
  # Status time series for stocks providing imports to the target country
  import_stocks_ts <- status %>%
    filter(stockid %in% import_stocks$stockid)
  
  # Calculate terminal year statistics
  stats <- import_stocks_ts %>% 
    filter(year==2015) %>% 
    rename(exp_iso3=iso3) %>% 
    mutate(iso3=iso3) %>% 
    group_by(iso3) %>% 
    summarize(imp_nstocks=n(),
              imp_bbmsy=mean(bbmsy_avg),
              imp_bbmsy_sd=sd(bbmsy_avg))
  
  # Return
  return(stats)
  
}

# Loop through ISOs and calculate mean import status
status_imports <- purrr::map_df(iso3s, function(x) mean_import_status(x))


# Step 3. Merge domestic and import status, calculate ratio, and plot
################################################################################

# Calculate percent difference between two values
perc_diff <- function(x1, x2){
  (x2-x1)/x1 *100
}

# Merge data, calculate ratio or percent difference
data <- status_home %>% 
  # Add import stock status
  left_join(status_imports, by="iso3") %>% 
  # Calculate and sort by percent difference in status
  mutate(pdiff=perc_diff(bbmsy, imp_bbmsy)) %>% 
  filter(!is.na(pdiff)) %>% 
  arrange(pdiff) %>% 
  # Add governance strength
  left_join(select(gov_orig, -country), by="iso3") %>% 
  mutate(strength=factor(strength, levels=c("weak", "moderate", "strong"))) %>% 
  rename(gov_strength=strength, gov_index=index) %>% 
  select(iso3, country, gov_strength, gov_index, everything())
  

# Plot
p <- ggplot(data, aes(x=pdiff, y=1:nrow(data), label=iso3, color=gov_strength)) +
  geom_point() +
  xlab("Percent difference in status\n(domestic relative to import stocks status") +
  ylab("") +
  geom_vline(xintercept=0, lty=3) +
  scale_color_discrete(name="Goveranance") +
  theme_bw()
p

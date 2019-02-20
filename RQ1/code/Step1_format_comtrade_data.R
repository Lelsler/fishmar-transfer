

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)


# Setup
################################################################################

# Packages
library(rio)
library(plyr)
library(dplyr)
library(tidyverse)

# Directories
inputdir <- "~/Nextcloud/FISHMAR-data/comtrade/raw/2012_comtrade"

# Read keys
iso3_key <- read.csv(file.path(inputdir, "country_code_baci12.csv"), as.is=T)

# Read HS6-FAO trade-FAO status key
prod_key_orig <- read.csv(file.path(inputdir, "hs6_fao_status_code_key.csv"), as.is=T)

# Format product key
prod_key <- prod_key_orig %>% 
  rename(hs12code=Commodity.HS2012code, hs12desc=Commodity.HS2012, fao_desc=Commodity,
         comm_name=Comm.Name, sci_name=Sci.Name.Orig, use=Usage) %>% 
  filter(use=="2_used")

# Setup
################################################################################

# Unzip 2012 COMTRADE files
zipfiles <- list.files(inputdir, pattern="baci12_")
if(F){lapply(zipfiles, function(x) unzip(file.path(inputdir, x), exdir=inputdir))}

# CSV files
csvfiles <- paste0("baci12_", 2012:2016, ".csv")

# Format files
data <- purrr::map_df(csvfiles, function(x){
  
  # Read data
  sdata <- read.csv(file.path(inputdir, x), as.is=T)
  # sdata <- read.csv(file.path(inputdir, csvfiles[1]), as.is=T)
  
  # Format data
  sdata1 <- sdata %>%
    # Rename columns
    rename(year=t, hs12code=hs6, value_usd=v, quantity_mt=q) %>% 
    # Add importing country ISO3
    left_join(select(iso3_key, i, iso3), by="i") %>% 
    rename(imp_iso3=iso3) %>% 
    # Add exporting country ISO3
    left_join(select(iso3_key, i, iso3), by=c("j"="i")) %>% 
    rename(exp_iso3=iso3) %>% 
    # Rearrange columns
    select(year, exp_iso3, imp_iso3, hs12code, value_usd, quantity_mt) %>% 
    # Filter to data with usable codes
    filter(hs12code %in% prod_key$hs12code)
  
  # Return
  return(sdata1)

})

# Export
saveRDS(data, file=file.path(inputdir, "2012-16_comtrade_hs12_data_use.Rds"))


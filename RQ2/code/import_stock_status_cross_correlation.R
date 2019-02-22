
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)


# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
statusdir <- "~/Nextcloud/FISHMAR-data/fao_stock_status"


# Read data
status_orig <- read.csv(file.path(statusdir, "1950_2017_FAO_bbmsy_timeseries_merge.csv"), as.is=T)


# Setup
################################################################################

# Build species key
taxa <- freeR::taxa(sort(unique(status_orig$sci_name)))

# Crab key
# Factor STOCKS by year of first exploitation
crabs <- status_orig %>% 
  filter(grepl("crab", comm_name)) %>% 
  group_by(stockid, fao_code, iso3, country, sci_name, comm_name) %>% 
  summarize(year=min(year[super<=0.8])) %>% 
  arrange(year) %>% 
  ungroup() %>% 
  mutate(stockid=factor(stockid, levels=stockid))
  
# Crab time series
sdata <- status_orig %>% 
  filter(grepl("crab", comm_name)) %>% 
  select(stockid, year, super) %>% 
  mutate(stockid=factor(stockid, levels=levels(crabs$stockid))) %>% 
  spread(key=stockid, value=super) %>% 
  select(-year)

# Calculate correlation matrix
corrmat <- cor(sdata, use="complete.obs")

# Plot correlation matrix
corrplot(corrmat, method="color", type="upper", diag=F, tl.cex=0.7, tl.col="black")


# ACF
acf(sdata)



# Function to compute cross-correlation matrix at defined time lag
mat <- sdata
lag <- -5
ccf_matrix <- function(mat, lag){
  
  # Setup output
  out <- matrix(data=NA, nrow=nrow(mat), ncol=ncol(mat))
  
  # Fill correlation matrix
  for(i in 1:ncol(mat)){
    for(j in 1:ncol(mat)){
      fdata <- na.omit(mat[,c(i,j)])
      ccf_out <- ccf(x=fdata[,1], y=fdata[,2], plot=F, type="correlation")
      ccf_df <- data.frame(lag=ccf_out$lag, acf=ccf_out$acf, n=ccf_out$n.used)
      corrval <- ccf_out$acf[ccf_out$lag==lag]
      if(length(corrval)==0){corrval <- NA}
      out[i,j] <- corrval
    }
  }
  
  # Return
  return(out)
  
}


corrmat_lag5 <- ccf_matrix(sdata, -5)

corrplot(corrmat_lag5, method="color", type="upper", diag=F, tl.cex=0.7, tl.col="black")






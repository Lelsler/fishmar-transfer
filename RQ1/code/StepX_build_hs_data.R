

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)


# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "/nfs/public-data/Baci"
outputdir <- "~/Nextcloud/FISHMAR-data/comtrade/raw/2012_comtrade"

key <- read.csv(file.path(inputdir, "country_code_baci12.csv"), as.is=T)

# Setup
################################################################################

# Unzip 2012 COMTRADE files
zipfiles <- list.files(inputdir, pattern="baci12_")
lapply(zipfiles, function(x) unzip(file.path(inputdir, x), exdir=outputdir))
csvfiles <- list.files(outputdir)

# Read files
for(i in 1:length(csvfiles)){
  sdata <- read.csv(file.path(outputdir, csvfiles[i]), as.is=T)
  sdata1 <- sdata %>%
    rename(year=t, hs6code=hs6, value=v, quantity=q, )
}






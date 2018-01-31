
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sp) # spTransform()
library(plyr)
library(dplyr)
library(rgdal) # readOGR()
library(rgeos) # gCentroid(), gArea()

# Directories
inputdir1 <- "data/fao_stock_status/raw/FAO_AREAS"
inputdir2 <- "data/fao_landings/raw/GlobalProuction_2017.1.1"
outputdir <- "data/fao_stock_status/raw"

# Projections
wgs84 <- CRS("+proj=longlat +ellps=WGS84")

# Read shapefile
mfa <- readOGR(dsn=inputdir1, layer="FAO_AREAS", verbose=F, stringsAsFactors=F)
mfa_info <- read.csv(paste(inputdir2, "CL_FI_AREA_GROUPS.csv", sep="/"), as.is=T)

# Build data
################################################################################

# Reduce to major fishing areas
mfa <- subset(mfa, F_LEVEL=="MAJOR")
plot(mfa)

# Calculate area centroids
centroids <- data.frame(coordinates(mfa))
colnames(centroids) <- c("long_dd", "lat_dd")
points(centroids, pch=16, col="red")

# Build dataframe
mfa_df <- cbind(mfa@data, centroids)
mfa_df1 <- mfa_df %>% 
  mutate(F_CODE=as.numeric(F_CODE)) %>% 
  left_join(select(mfa_info, Code, Name_en), by=c("F_CODE"="Code")) %>% 
  select(OCEAN, F_CODE, Name_en, long_dd, lat_dd) %>% 
  rename(fao_code=F_CODE, ocean=OCEAN, fao_area=Name_en)


# Export data
################################################################################

# Export data
write.csv(mfa_df1, paste(outputdir, "FAO_major_fishing_area_centroids.csv", sep="/"), row.names=F)













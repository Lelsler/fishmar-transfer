

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(plyr)
library(dplyr)
library(freeR)
library(maptools)
library(RColorBrewer)

# Directories
datadir <- "~/Nextcloud/FISHMAR-data/wb_indicators"


# Function to format data
################################################################################

# Function
indicator <- "RuleofLaw"
format_data <- function(indictator){
  
  # Read data
  data_orig <- import(file.path(datadir, "wgidataset.xlsx"), sheet=indicator)
  
  # Remove header
  # (but retain 2 column name rows)
  data <- data_orig %>% 
    slice(13:n())
  
  # Build and add columns names
  # Use 2 columns name rows to build column names then remove
  cols <- paste(data[1,], data[2,], sep="_")
  colnames(data) <- cols
  data <- slice(data, 3:n())
  
  # Grab country info
  countries <- data$`NA_Country/Territory`
  isos <- data$NA_WBCode
  
  # Isolate and format estimates
  # Replace NA values, convert to numeric, remove lingering column, rename columns
  ests <- data[,grepl("Estimate", colnames(data))]
  ests[ests=="#N/A"] <- NA
  ests <- sapply(ests, as.numeric)
  ests <- ests[,!grepl("#N/A", colnames(ests))]
  colnames(ests) <- gsub("_Estimate", "", colnames(ests))
  
  # Add countries
  ests_wide <- data.frame(iso3=isos, country=countries, ests, stringsAsFactors=F)
  
  # Reshape wide-to-long
  ests_long <- reshape2::melt(ests_wide, id.vars=c("iso3", "country"), 
                              variable.name="year", value.name="est") %>% 
    mutate(year=as.numeric(gsub("X", "", year)))
  
  # Return
  return(ests_long)
  
}


# Format and merge data
################################################################################

# Format data
# rule of law, control of corruption, governmental effectiveness, and regulatory quality
law <- format_data("RuleofLaw")
corr <- format_data("ControlofCorruption")
gov <- format_data("GovernmentEffectiveness")
reg <- format_data("RegulatoryQuality")

# Merge data
data <- law %>% 
  rename(law=est) %>% 
  # Add control of corruption
  left_join(select(corr, -country), by=c("iso3", "year")) %>% 
  rename(corr=est) %>% 
  # Add governmental effectiveness
  left_join(select(gov, -country), by=c("iso3", "year")) %>% 
  rename(gov=est) %>% 
  # Add regulatory quality
  left_join(select(reg, -country), by=c("iso3", "year")) %>% 
  rename(reg=est) %>% 
  # Calculate mean index
  mutate(gov_index=rowMeans(select(., law, corr, gov, reg), na.rm=T))

# Calculate average
results <- data %>% 
  group_by(iso3, country) %>% 
  summarize(law=mean(law, na.rm=T),
            corr=mean(corr, na.rm=T),
            gov=mean(gov, na.rm=T),
            reg=mean(reg, na.rm=T),
            gov_index=mean(gov_index, na.rm=T)) %>% 
  arrange(gov_index) %>% 
  mutate(gov_strength=cut(gov_index, breaks=c(-2.5, 0, 1.25, 2.5), labels=c("weak", "moderate", "strong"))) %>% 
  ungroup()

# Table
table(results$gov_strength)

# Visualize results
################################################################################

# Build spatial sample size data
data(wrld_simpl) # from maptools
world_orig <- wrld_simpl@data
world <- world_orig %>% 
  left_join(results, by=c("ISO3"="iso3")) %>% 
  mutate(index_bin=cut(gov_index, breaks=seq(-2.5, 2.5, 0.25)),
         index_color=colorpal(brewer.pal(9,"RdYlBu"), n=nlevels(index_bin))[index_bin],
         index_color=ifelse(is.na(index_color), "grey90", index_color),
         catg_color=revalue(gov_strength, c("strong"=brewer.pal(9,"RdYlBu")[8], 
                                        "moderate"=brewer.pal(9,"RdYlBu")[4],
                                        "weak"=brewer.pal(9,"RdYlBu")[2])),
         catg_color=ifelse(is.na(catg_color), "grey90", as.character(catg_color)))
wrld_simpl@data <- world

# Plot data
par(mfrow=c(2,1), mar=c(0,0,0,0))
plot(wrld_simpl, col=wrld_simpl$index_color, border="grey30", lwd=0.3)
plot(wrld_simpl, col=wrld_simpl$catg_color, border="grey30", lwd=0.3)


# Export
################################################################################

# Export results
write.csv(results, file.path(datadir, "world_bank_composite_governance_index.csv"), row.names=F)



# Packages
library(tidyverse)

# Directories
#datadir <- "~/Nextcloud/FISHMAR-data/rq2/test_preferential" # MNO
datadir <- "~/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential" # LGE

# Read data
data_orig <- read.csv(file.path(datadir, "CT_fish_trade92.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  rename(year=t, hs92desc=Shortdescription.HS1992, hs92code=Code.HS1992,
         exp_iso3=iso3, imp_iso3=imp.iso3, value_usd=v, quantity_mt=q, group=group_name) %>% 
  select(imp_iso3, group, hs92code, hs92desc, exp_iso3, year, value_usd, quantity_mt) %>% 
  arrange(imp_iso3, group, hs92code, hs92desc, exp_iso3, year)
  

# Build data
################################################################################

# Groups
groups <- sort(unique(data$group))

# Loop through groups
g <- "crab"
# results <- purrr:::map_df(groups, function(g) {
for(i in 1:length(groups)){
  
  # Subset group data
  gdata <- data %>% 
    # Reduce to target group
    filter(group==groups[i]) %>% 
    # Reduce to unique links by year
    select(imp_iso3, exp_iso3, year) %>% 
    unique() %>% 
    mutate(link_id=paste(imp_iso3, exp_iso3, sep="-"))
  
  # Loop through years
  years <- sort(unique(gdata$year))
  years <- years[1:(length(years)-1)]
  out <- purrr::map_df(years, function(t){
    
    # Subset t and t+1
    links_t0 <- gdata$link_id[gdata$year==t]
    links_t1 <- gdata$link_id[gdata$year==(t+1)]
    links_deleted <- sum(!links_t1 %in% links_t0)
    links_added <- sum(!links_t0 %in% links_t1)
    links_shared <- sum(links_t1 %in% links_t0)
    link_turnover <- links_added + links_deleted
    link_turnover1 <- (length(links_t1)-links_shared) + (length(links_t0)-links_shared)
    
    # Merge into dataframe
    df <- data.frame(group=groups[i], year=t, n_add=links_added, n_lost=links_deleted,
                     n_shared=links_shared, n_turnover=link_turnover)
    return(df)
    
  })
  
  # Return data
  # return(out)
  if(i==1){results <- out}else{results <- rbind(results, out)}
  
}
  
# })

# save results file
write.csv(results, file.path(datadir, "link_turnover.csv"))


turnover_avg <- results %>% 
  group_by(group) %>% 
  summarize(beta_avg=mean(n_turnover)) %>% 
  arrange(desc(beta_avg))
  


p <- ggplot(results, aes(x=year, y=n_turnover, group=group, col=group)) +
  geom_line()
p

plot(n_turnover ~ year, d, type="l")


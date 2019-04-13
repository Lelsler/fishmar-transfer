
# Packages
library(tidyverse)
library(igraph)
library(ineq)

# Directories
datadir <- "~/Nextcloud/FISHMAR-data/rq2/test_preferential"

# Read data
data_orig <- read.csv(file.path(datadir, "CT_fish_trade92.csv"), as.is=T)

ancho_test = subset(data, group == "anchovies")

ancho_test = ancho_test %>% group_by(imp_iso3) %>% summarise(sum_imp = sum(quantity_mt)) %>%
  arrange(desc(sum_imp)) %>% filter(!is.na(sum_imp)) 

%>% 
    mutate(percentage_imp_total = sum_imp/sum(sum_imp))
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
#results <- purrr:::map_df(groups, function(g) {
for(i in 1:length(groups)){
  
  # Subset group data
  gdata <- data %>% 
    # Reduce to target group
    filter(group==groups[i]) %>% 
    # Reduce to unique links by year
    select(imp_iso3, exp_iso3, year, quantity_mt) %>% 
    distinct() #%>% 
    #mutate(link_id=paste(imp_iso3, exp_iso3, sep="-"))
  
  # Loop through years
  years <- sort(unique(gdata$year))
  out <- purrr::map_df(years, function(t){
    
    # Subset t 
    gdatat <- gdata[gdata$year==t, ]
    edgelist = gdatat %>%select(imp_iso3, exp_iso3, quantity_mt)
    edgelist = na.omit(edgelist)
    gf = graph_from_data_frame(edgelist, directed = TRUE)
    global = transitivity(gf, type ="global")
    
    #sum trades per importer and calculate gini
    ginidat = gdatat %>% select(imp_iso3, quantity_mt) %>%
      group_by(imp_iso3) %>% summarise(sum_mt = sum(quantity_mt))
    
    gini = ineq(ginidat$sum_mt,type="Gini")
   
    # Merge into dataframe
    df <- data.frame(group=groups[i], year=t, clustering = global, gini=gini)
    return(df)
    
  })
  
  # Return data
  # return(out)
  if(i==1){results <- out}else{results <- rbind(results, out)}
  
}
  
# })

#turnover_avg <- results %>% 
  #group_by(group) %>% 
  #summarize(beta_avg=mean(n_turnover)) %>% 
  #arrange(desc(beta_avg))

results = results %>% filter(group == "anchovies" | group == "cod" |group == "lobster" | group == "crab" | group == "mackerel")


p <- ggplot(results, aes(x=year, y=gini, group=group, col=group)) +
  geom_line()+ scale_y_continuous(limits = c(0, 1))
p




p1 <- ggplot(results, aes(x=year, y=clustering, group=group, col=group)) +
  geom_line() + scale_y_continuous(limits = c(0, 1))
p1
#plot(n_turnover ~ year, d, type="l")

#write.csv(results, "gini_and_clustering.csv")



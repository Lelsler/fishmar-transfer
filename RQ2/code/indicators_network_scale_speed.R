#clear workspace
rm(list= ls())

# Packages
library(tidyverse)
library(igraph)
library(ineq)
library(sjstats)
library(glmmTMB)

# Directories
datadir <- "~/Nextcloud/FISHMAR-data/rq2/test_preferential"
datadir2 = "~/Nextcloud/FISHMAR-data/rq2/trade_duration_AG"
datadir3 = "~/Nextcloud/FISHMAR-data/fao_stock_status"
datadir4 = "~/Nextcloud/FISHMAR-data/comtrade/processed/timeseries"

# Read data
data_orig <- read.csv(file.path(datadir, "CT_fish_trade92.csv"), as.is=T)

#stocks  = read.csv(file.path(datadir3, "1950_2017_FAO_bbmsy_timeseries_merge.csv"), as.is=T)
#match = read.csv(file.path(datadir4,"matchingstocksHS92.csv"), as.is=T)


trade = trade %>% select(Shortdescription.HS1992, group_name)%>%
  distinct()




# Format data
################################################################################

# Format data
data <- data_orig %>% 
  rename(year=t, hs92desc=Shortdescription.HS1992, hs92code=Code.HS1992,
         exp_iso3=iso3, imp_iso3=imp.iso3, value_usd=v, quantity_mt=q, group=group_name) %>% 
  select(imp_iso3, group, hs92code, hs92desc, exp_iso3, year, value_usd, quantity_mt) %>% 
  arrange(imp_iso3, group, hs92code, hs92desc, exp_iso3, year)

match = match %>% select(Shortdescription.HS1992, comm_name, sci_name)%>% 
  rename(hs92desc=Shortdescription.HS1992) 

match = data %>% left_join(match)

stocks = stocks %>% select(sci_name, comm_name, super, year) %>% 
  left_join(match) %>% filter(!is.na(super))%>% filter(!is.na(hs92desc))
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
    distinct() %>% 
    mutate(link_id=paste(imp_iso3, exp_iso3, sep="-"))
  
  # Loop through years
  years <- sort(unique(gdata$year))
  out <- purrr::map_df(years, function(t){
    
    # Subset t 
    gdatat <- gdata[gdata$year==t, ]
    edgelist = gdatat %>%select(imp_iso3, exp_iso3, quantity_mt)
    edgelist = na.omit(edgelist)
    g = graph_from_data_frame(edgelist, directed = TRUE)
    global = transitivity(g, type ="global")
    
    nodeList = data.frame(iso3 = igraph::V(g)$name, 
                          ID = c(0:(igraph::vcount(g) - 1))) # make nodelist with ID number
    nodeList = cbind(nodeList, nodeDegree=igraph::degree(g, v = igraph::V(g), mode = "all"), i)
    nodeList = cbind(nodeList, nodeDegree.in=igraph::degree(g, v = igraph::V(g), mode = "in")) #calculate node degree for all nodes in network
    
    average_degree = mean(nodeList$nodeDegree)
    
    ###########################gini, no longer needed???
    #sum trades per importer and calculate gini 
    #ginidat = gdatat %>% select(imp_iso3, quantity_mt) %>%
     #group_by(imp_iso3) %>% summarise(sum_mt = sum(quantity_mt))
    
   # gini = ineq(ginidat$sum_mt,type="Gini")
    
    
    # Subset t and t+1
    links_t0 <- gdata$link_id[gdata$year==t]
    links_t1 <- gdata$link_id[gdata$year==(t+1)]
    links_deleted <- sum(!links_t1 %in% links_t0)
    links_added <- sum(!links_t0 %in% links_t1)
    links_shared <- sum(links_t1 %in% links_t0)
    link_turnover <- links_added + links_deleted
    link_turnover1 <- (length(links_t1)-links_shared) + (length(links_t0)-links_shared)
    
    df <- data.frame(group=groups[i], year=t, n_add=links_added, n_lost=links_deleted,
                     n_shared=links_shared, n_turnover=link_turnover,clustering = global, av_degree = average_degree)
    
    df = merge(nodeList, df)
    # Merge into dataframe
    return(df)
    
  })
  
  # Return data
  # return(out)
  if(i==1){results <- out}else{results <- rbind(results, out)}

}

results = results %>%
  select(-i, - ID) 
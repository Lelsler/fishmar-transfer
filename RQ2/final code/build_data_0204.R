# this file builds the data set for the analysis
# clear workspace
rm(list = ls())
graphics.off()

# Packages
library(tidyverse)
library(igraph)
library(ineq)
library(sjstats)
library(glmmTMB)
library(data.table)
library(base)
require(dplyr)

# Directories
datadir <- "~/Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential" #LGE
datadir2 = "~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data" #LGE
datadir3 = "~/Documents/SESYNC/Files/FISHMAR-data/fao_stock_status" #LGE
datadir4 = "~/Documents/SESYNC/Files/FISHMAR-data/comtrade/processed/timeseries" #LGE


# Read data
data_orig <- read.csv(file.path(datadir, "CT_fish_trade92.csv"), as.is=T)
stocks  = read.csv(file.path(datadir3, "1950_2017_FAO_bbmsy_timeseries_merge.csv"), as.is=T)
match = read.csv(file.path(datadir4,"matchingstocksHS92.csv"), as.is=T)
trade_duration <- read.csv(file.path(datadir2, "trade_duration.csv"), as.is=T)

######################################  Format data  ##########################################
# Format data
data <- data_orig %>% 
  rename(year=t, hs92desc=Shortdescription.HS1992, hs92code=Code.HS1992,
         exp_iso3=iso3, imp_iso3=imp.iso3, value_usd=v, quantity_mt=q, group=group_name) %>% 
  select(imp_iso3, group, hs92code, hs92desc, exp_iso3, year, value_usd, quantity_mt) %>% 
  arrange(imp_iso3, group, hs92code, hs92desc, exp_iso3, year)

match = match %>% select(Shortdescription.HS1992, comm_name, sci_name)%>% 
  rename(hs92desc=Shortdescription.HS1992) 

match = data %>% left_join(match)

stocks = stocks %>% select(sci_name, comm_name, super, year, iso3) %>% 
  rename(exp_iso3 = iso3)%>% left_join(match) %>% 
  filter(!is.na(super))%>%filter(!is.na(hs92desc)) %>%
  select(exp_iso3, group, year, super)


### remove sci_name multiple matches to group
# new df
observation <- data.frame(sci_name = match$sci_name, group = match$group) # new df including sciname,group
# remove all spp we will rm later
remove = c('salmon','salmonidae','shrimp','tuna') # vector with groups to be removed
t1 = subset(observation,!(group %in% remove)) # remove groups
# unique pairs
t2 <- unique(t1[c("sci_name","group")]) # new df incl only unique values of sci_name group pairs
# check 
t3 <- t2[1] # used in next line
t4 <- subset(t2, (duplicated(t3))) # new df incl only duplicate observations of sci_name
# now choose all duplicates and remove them from the match data after line 40 
t5 <- unique(t4[c("sci_name")]) # 
t6 = t2[(t2$sci_name %in% t5$sci_name),] # identify sci_name with double matches to group
# identified wrong matches
fal <- data.frame("sci_name" = c("Homarus americanus","Homarus gammarus","Jasus edwardsii","Jasus lalandii","Jasus novaehollandiae","Jasus paulensis","Jasus paulensis","Jasus tristani","Jasus tristani","Metanephrops challengeri","Palinurus elephas","Palinurus gilchristi","Panulirus argus","Panulirus argus","Panulirus cygnus","Panulirus homarus","Paralichthys olivaceus","Reinhardtius hippoglossoides","Thenus orientalis"),"group" = c("lobster","lobster","lobster","lobster","homarus","homarus","flatfish","homarus","flatfish","homarus","homarus","homarus","lobster","flatfish","lobster","homarus","flatfish","flatfish","homarus"))

# new columns to remove the wrong matches
match$grsci <- paste(match$group, match$sci_name, sep="_") 
fal$grsci <- paste(fal$group, fal$sci_name, sep="_")

match = match[!(match$grsci %in% fal$grsci),] # identify sci_name with double matches to group


######################################  Build data  ##########################################
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
    nodeList = cbind(nodeList, nodeDegree=igraph::degree(g, v = igraph::V(g), mode = "all"), i) # calculate total # of links of each node in network
    nodeList = cbind(nodeList, nodeDegree.in=igraph::degree(g, v = igraph::V(g), mode = "in")) # calculate total # of exporters links to importers 

    average_degree = mean(nodeList$nodeDegree)
    
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

######################################  Merge data  ##########################################

# prepare results data
results = results %>%
  select(-i, - ID) 

results <- results %>% rename(exp_iso3 = iso3)

# leftjoin trade duration file
trade_duration <- trade_duration %>% rename(group = group_name)%>% rename(actual_duration = dur)%>%
  select(-X) %>% select(exp_iso3, year, group, imp_iso3, actual_duration)

# average trade duration
avg_duration = aggregate(trade_duration[, 5], list(trade_duration$exp_iso3, trade_duration$year, trade_duration$group), mean)
setnames(avg_duration, old = c('Group.1','Group.2','Group.3','x'), new = c('exp_iso3','year','group','avg_duration'))

# left join data sets
volatility_duration = results %>% left_join(avg_duration)
volatility_duration = volatility_duration %>% select(exp_iso3, group, year, nodeDegree, nodeDegree.in, av_degree, n_turnover, clustering, avg_duration)
volatility_duration = na.omit(volatility_duration)

#### remove aquaculture and environmental variability driven species
# check which groups are there
check = volatility_duration %>% select('group') 
check= unique(check)

# spp to remove
remove = c('salmon','salmonidae','shrimp','tuna') #vector with groups to be removed
volatility_duration_clean = subset(volatility_duration,!(group %in% remove))

### leftjoin data and stocks
# calculate mean of super
stocks_edited <-stocks %>%
  group_by(exp_iso3, group, year) %>%
  summarize(mean(unique(super)))

# leftjoin data and stocks
volatility_duration_stocks = volatility_duration_clean %>% left_join(stocks_edited) %>%
  distinct()

# save data 
#write.csv(volatility_duration_stocks, "~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data/RQ2_data_covariates_0204.csv")

final_data <- read.csv(file.path(datadir2, "RQ2_data_covariates_0204.csv"), as.is=T)




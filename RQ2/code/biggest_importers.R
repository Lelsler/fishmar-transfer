
# Packages
library(tidyverse)
library(igraph)
library(ineq)

# Directories
datadir <- "~/Nextcloud/FISHMAR-data/rq2/test_preferential"

# Read data
data_orig <- read.csv(file.path(datadir, "CT_fish_trade92.csv"), as.is=T)


# Format data
data <- data_orig %>% 
  rename(year=t, hs92desc=Shortdescription.HS1992, hs92code=Code.HS1992,
         exp_iso3=iso3, imp_iso3=imp.iso3, value_usd=v, quantity_mt=q, group=group_name) %>% 
  select(imp_iso3, group, hs92code, hs92desc, exp_iso3, year, value_usd, quantity_mt) %>% 
  arrange(imp_iso3, group, hs92code, hs92desc, exp_iso3, year)

groups <- sort(unique(data$group))
# Loop through groups
#results <- purrr:::map_df(groups, function(g) {
for(i in 1:length(groups)){
  
  # Subset group data
  gdata <- data %>% 
    # Reduce to target group
    filter(group==groups[i]) %>% group_by(imp_iso3) %>% summarise(sum_imp = sum(quantity_mt)) %>%
        arrange(desc(sum_imp)) %>% filter(!is.na(sum_imp)) %>% 
             mutate(percentage_imp_total = sum_imp/sum(sum_imp)) %>%
                mutate(rank = dense_rank(desc(sum_imp)))

ranks = unique(gdata$rank)

out <- purrr::map_df(ranks, function(t){
  
  sum_rank <- gdata %>% 
    # Reduce to target group
    filter(rank<= t) %>%
    summarise(sum_rank = sum(percentage_imp_total))
    # Reduce to unique links by year

  df <- data.frame(group=groups[i], rank=t, sum_rank = sum_rank)
  return(df)
  
})
# Format data
################################################################################
if(i==1){results <- out}else{results <- rbind(results, out)}

}

results = subset(results, rank < 17)

biggest3= subset(results, rank <4) %>% rename(percentage = sum_rank)
onlybiggest3 = subset(biggest3, rank == 3) %>% arrange(desc(percentage))
############################################################################
data %>% group_by(group) %>%
  summarise(number_importers = length(unique(imp_iso3))) %>% head()

ggplot(biggest3, aes(x=rank, y=percentage, colour = group)) + 
  geom_bar(stat="identity")+ facet_wrap(~group) + ggtitle("percentage commodity imported by biggest importers")

levels(onlybiggest3$group)
onlybiggest3$group <- reorder(onlybiggest3$group, rowSums(onlybiggest3[-1]))


ggplot(onlybiggest3, aes(x=group, y=percentage, colour = group)) + 
  geom_bar(stat="identity") + 
  ggtitle("percentage commodity imported by 3 biggest importers") +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))




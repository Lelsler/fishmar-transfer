rm(list = ls())

setwd("/Users/mtn1/Nextcloud/FISHMAR-data/rq2/figures")

# libraries
library(tidyverse)


datadir = "~/Nextcloud/FISHMAR-data/fao_stock_status"
datadir2 = "~/Nextcloud/FISHMAR-data/comtrade/processed/timeseries"
datadir3 <- "~/Nextcloud/FISHMAR-data/rq2/test_preferential"
datadir4 <- "~/Nextcloud/FISHMAR-data/rq2/summary_stats_networks"


stocks  = read.csv(file.path(datadir, "1950_2017_FAO_bbmsy_timeseries_merge.csv"), as.is=T)
match = read.csv(file.path(datadir2,"matchingstocksHS92.csv"), as.is=T)
trade <- read.csv(file.path(datadir3, "CT_fish_trade92.csv"), as.is=T)
gini_clustering = read.csv(file.path(datadir4, "gini_and_clustering.csv"), as.is=T)

trade = trade %>% select(Shortdescription.HS1992, group_name)%>%
  distinct()

match = match %>% select(Shortdescription.HS1992, comm_name, sci_name)%>% 
  left_join(trade) %>% filter(!is.na(group_name))

stocks = stocks %>% select(sci_name, comm_name, super, year) %>% 
  left_join(match)%>% rename(group = group_name)%>% 
  left_join(gini_clustering) %>% filter(!is.na(super)) %>%
  filter(!is.na(gini))

cor(stocks$super, stocks$gini)
cor(stocks$super, stocks$clustering)

plot(stocks$clustering, stocks$super)

stocks$clustering= scale(stocks$clustering)
stocks$gini= scale(stocks$gini)

library(lme4)
super$year = as.factor(super$year)
linear = lmer(super ~ clustering + gini+(1|group) + (1|year), data = stocks)

summary(linear)


ggplot(stocks, aes(x=clustering, y= super))+geom_point() + geom_smooth(method=lm)
ggplot(stocks, aes(x=gini, y= super))+geom_point() + geom_smooth()

summary(lm(stocks$super ~ stocks$clustering))
summary(lm(stocks$super ~ stocks$gini))

#importer dominated
homarus_rock = stocks %>% filter(group_name == "homarus" | group_name == "rocklobster") %>%
  filter(year > 1992) %>% distinct() %>% group_by(year, comm_name, group_name) %>% 
  summarise(mean_super= mean(super))

summary_homarus_rock= homarus_rock %>% group_by(year, group_name) %>% 
  summarise(mean_super_all_stocks = mean(mean_super), sd_super = sd(mean_super))



library(Hmisc)
ggplot(homarus_rock, aes(x=year, y=mean_super, colour = comm_name)) +
  geom_point()+ facet_wrap(~group_name) + geom_smooth()


ggplot(summary_homarus_rock, aes(x=year)) + 
  geom_line(aes(y=mean_super_all_stocks), colour="black") + 
  geom_ribbon(aes(ymin=(mean_super_all_stocks-sd_super), ymax=(mean_super_all_stocks+sd_super)), alpha=0.2)+ facet_wrap(~group_name)


# dispersed 
mack_sard = stocks %>% filter(group_name == "mackerel" | group_name == "sardines") %>%
  filter(year > 1992) %>% distinct() %>% group_by(year, comm_name, group_name) %>% 
  summarise(mean_super= mean(super))

ggplot(mack_sard , aes(x=year, y=mean_super, color=comm_name)) +
  geom_line()+ facet_wrap(~group_name)

summary_mack_sard= mack_sard %>% group_by(year, group_name) %>% 
  summarise(mean_super_all_stocks = mean(mean_super), sd_super = sd(mean_super))

ggplot(summary_mack_sard, aes(x=year)) + 
  geom_line(aes(y=mean_super_all_stocks), colour="black") + 
  geom_ribbon(aes(ymin=(mean_super_all_stocks-sd_super), ymax=(mean_super_all_stocks+sd_super)), alpha=0.2)+ facet_wrap(~group_name)


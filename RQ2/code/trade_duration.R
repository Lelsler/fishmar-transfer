#script to calculate trade duration for total trades

#clear workspace
rm(list = ls())
graphics.off()

#setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/")

library(tidyverse)

datadir = "~/Nextcloud/FISHMAR-data/rq2/trade_duration_AG"
duration <- read.csv(file.path(datadir, "exports_test.csv"), as.is=T) # trade duration


### new names
duration = duration %>%
  rename(imp_iso3 = imp.iso3, exp_iso3 = iso3, new_export = new.export, value = total.value, 
         quantity = total.quantity, year = t)%>%
  select(-X)%>%
  mutate(imp_exp = paste0(imp_iso3, "_", exp_iso3))


groups <- sort(unique(duration$group_name))



#for (i in 1:length(groups)){
  # Subset group data
  
i =1
data <- duration %>% 
    # Reduce to target group
    filter(group_name==groups[i]) 
    
    couples = sort(unique(data$imp_exp))
  
    for (a in length(couples)){
      
      gdata <- data%>% 
        # Reduce to target group
        filter(imp_exp==couples[a])   
      
      years = sort(unique(gdata$year))
      
      out <- purrr::map_df(years, function(t){
        
      trade_t0 <- gdata[gdata$year==t,]
      trade_t1 <- gdata[gdata$year==t+1, ]
      
      if (length(trade_t1$duration)>0){
        if(trade_t1$duration > trade_t0$duration){new_duration = trade_t1$duration}else{new_duration = trade_t0$duration} 
      }
      
      if(length(trade_t1$year)==0){new_duration = 1}
      
      df = data.frame(t, duration = new_duration, gdata$imp_exp, group = groups[i])
      return(df)
      
      
    })
      
    }

#}



duration %>% group_by(imp_exp, group_name)



#script to calculate trade duration for total trades

#clear workspace
rm(list = ls())
graphics.off()

#setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/")

library(tidyverse)
library(magicfor)

datadir = "~/Nextcloud/FISHMAR-data/rq2/trade_duration_AG"
duration <- read.csv(file.path(datadir, "exports_test.csv"), as.is=T) # trade duration


### new names
duration = duration %>%
  rename(imp_iso3 = imp.iso3, exp_iso3 = iso3, new_export = new.export, value = total.value, 
         quantity = total.quantity, year = t)%>%
  select(-X)%>%
  mutate(imp_exp = paste0(imp_iso3, "_", exp_iso3))%>%
  arrange(year)%>% 
  filter(!is.na(exp_iso3), !is.na(imp_iso3))





groups <- sort(unique(duration$group_name))


for (i in 1:length(groups)){
  # Subset group data
  
data <- duration %>% 
    # Reduce to target group
    filter(group_name==groups[i]) 
    
    couples = sort(unique(data$imp_exp))
  
    for (a in 1: length(couples)){
      
      gdata <- data%>% 
        # Reduce to target group
        filter(imp_exp==couples[a])%>%
        select(-value, -quantity)
      
      years = sort(unique(gdata$year))
      index = 1
      
      #out <- purrr::map_df(v_years, function(t){
        
        for (t in c(1:length(years))){
          
          #t = 2
          
          links0 <- gdata[gdata$year==years[t-1], ]
          links <- gdata[gdata$year==years[t], ]
          
          links = na.omit(links)
          links0 = na.omit(links0)
          
          #if the duration of trade in the previous time step was bigger than in the current THIS time step is a new trade
          
          if(length(links0$new_export) >0){
            if(links$duration < links0$duration | links0$duration ==  links$duration){index = index +1}
          }else{index = index}
          
          df = data.frame(trade_ID = index, duration = links$duration, year = years[t], group= groups[i], imp_exp =  couples[a])
          print(df)
          
          out_put = if(a==1 & t == 1 & i == 1){out_put = df}else{out_put = bind_rows(out_put, df)}
        }
      
    }
}

out_put





    duration %>% group_by(group_name, imp_exp)%>%
      arrange(year)%>% summarise(difftime= difftime(lead(year), year))


try = subset(duration, group_name == "anchovies" & imp_exp == "ABW_USA")

years = try$year

index = 1

for (t in 1:length(years)){

  links <- try[try$year==years[t], ]
  links1 <- try[try$year==years[t+1], ]
  
  links = na.omit(links)
  links1 = na.omit(links1)
  
  if(length(links1$new_export) >0){
  if(links1$duration < links$duration | links1$duration ==  links$duration){index = index +1}
  }else{index = index}
  
  print(index)
}


  

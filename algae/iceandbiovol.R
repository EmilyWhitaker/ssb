#snow and ice thickness vs total biovolume, get duration in here too

library(tidyverse)
library(lubridate)

icey <- read_csv("iceduraation.csv") %>%
  filter(lakeid== 'SP')


tooiceytooice <- read_csv("snowicedepth.csv") %>%
  filter(lakeid== 'SP')


#find ice on and off- these could be done differently 
icetime_sp <- tooiceytooice %>% 
  mutate(Vol.Tot.Ice = Vol.Blue.Ice+ Vol.White.Ice,
         Tot.Ice.Thickness = White.Ice.Thickness+Blue.Ice.Thickness,
         #volume of ice before and after date
         lead.ice = lead(Vol.Tot.Ice),
         lag.ice = lag(Vol.Tot.Ice),
         #if there was ice today and no ice the day after ==>ice off
         iceoff = ifelse(lead.ice==0&Vol.Tot.Ice!=0, 1, 0),
         #if there was no ice the day before and now there is ==> ice on
         iceon = ifelse(lag.ice==0&Vol.Tot.Ice!=0,1,0),
         date_time = as.Date(time)) %>% 
  select(date_time, iceon, iceoff) %>% 
  #convert to long format
  gather(var, val, contains("iceo")) %>% 
  #remove dates where there wasn't a change in ice
  filter(val==1) %>% 
  arrange(date_time) %>% 
  mutate(year = year(date_time),
         winter = ifelse(var == "iceoff", year, lead(year))) %>% 
  select(winter, var, date_time) %>% 
  #spread back to wide format
  spread(key = var, value = date_time) %>% 
  mutate(duration = iceoff-iceon) 

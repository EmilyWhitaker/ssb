library(tidyverse)
library(lubridate)
library(ggplot2)

surface<- read_csv('chloros.csv') %>%
  filter(depth == '0') 


integrated <- read_csv("Emilychlorophyll.csv") 

bothchloro <- integrated %>%
  right_join(surface, by= c('sampledate'))
write.csv(bothchloro, 'fullchloro')

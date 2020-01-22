library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)


chloro_all <- read_csv("chloro_all.csv", 
                       col_types = cols(daynum = col_skip(), 
                                        flagchlor = col_skip(), flagphaeo = col_skip(), 
                                        phaeo = col_skip(), sampledate = col_date(format = "%Y-%m-%d"))) 

chloro_all <- chloro_all %>%
  filter(depth == '0') %>%
  filter(lakeid== 'SP') %>%
  rename(
    surfchlor = chlor
  )



Emilychlorophyll <- read_csv("Emilychlorophyll.csv", 
                             col_types = cols(sampledate = col_date(format = "%m/%d/%Y"), 
                                              yearfrac = col_skip()))

bothchloro <- Emilychlorophyll %>%
  right_join(chloro_all, by= c('sampledate'))
write.csv(bothchloro, 'fullchloro.csv')


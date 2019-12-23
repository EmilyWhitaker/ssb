#### Seperating chems by layers ####

library(lubridate)
library(ggplot2)
library(tidyverse)
library(readr)

o2chems <- read_csv("o2chems2.csv", 
                    col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))
View(o2chems)
#already has groups

#o2chems <- 
#  o2chems %>%
#  mutate(
#    watercol= cut(depth, 
#               breaks = c(-Inf, 3, 11, Inf),
#               lables= c('0-3', '3-11', '11-18')
#              )
#    )
#write.csv(o2chems, "o2andchemlayered.csv")


layerso2chems <- read_csv("o2andchemlayered.csv", 
                    col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))
View(layerso2chems)






al_data <-
  al_data %>%
  mutate(
    yrs= cut(year,
             breaks =  c(-Inf, 10, 80, 90, Inf),
             labels =  c("2000s", "2010s", "1980s","1990s")
    )
  )






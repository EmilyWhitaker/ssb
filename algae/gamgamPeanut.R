library(tidyverse)
library(lubridate)
library(ggplot2)
library(mgcv)

Peanutyears <- read_csv("Peanut_lte_lite3_chloro_datayearonly.csv", 
                    col_types = cols(X1 = col_skip(), X1.x = col_skip(), 
                                     daynum = col_skip(), sampledate = col_date(format = "%m/%d/%Y")))

##Add in daynumber        
Peanutyears$daynum<- yday(Peanutyears$sampledate)
Peanutyears
######daynumb

Peanutexperience <- gam(CellBioVol ~ s(year4.x, bs = "gp")+ s(daynum), data=Peanutyears)
Peanutplot<-plot(Peanutexperience, page=1)
Peanutplot
summary(Peanutexperience)
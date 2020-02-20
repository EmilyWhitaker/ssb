library(tidyverse)
library(lubridate)
library(ggplot2)
library(mgcv)

NDyears <- read_csv("ND_lte_lite3_chloro_datayearonly.csv", 
                             col_types = cols(X1 = col_skip(), X1.x = col_skip(), 
                                              daynum = col_skip(), sampledate = col_date(format = "%m/%d/%Y")))

##Add in daynumber        
NDyears$daynum<- yday(NDyears$sampledate)
NDyears
######daynumb

NDexperience <- gam(CellBioVol ~ s(year4.x, bs = "gp")+ s(daynum), data=NDyears)
plot<-plot(NDexperience, page=1)
plot
summary(NDexperience)


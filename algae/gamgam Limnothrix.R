library(tidyverse)
library(lubridate)
library(ggplot2)
library(mgcv)

Limnothrixyears <- read_csv("Limnothrixlte_lite3_chloro_datayearonly.csv", 
                       col_types = cols(X1 = col_skip(), X1.x = col_skip(), 
                                        daynum = col_skip(), sampledate = col_date(format = "%m/%d/%Y")))
View(Limnothrixyears)

##Add in daynumber        
Limnothrixyears$daynum<- yday(Limnothrixyears$sampledate)
Limnothrixyears
######daynumb

Limnothrixexperience <- gam(CellBioVol ~ s(year4.x, bs = "gp")+ s(daynum), data=Limnothrixyears)
limn<-plot(Limnothrixexperience, page=1)
limn<- plot(Limnothrixexperience,pages=1,residuals=TRUE,all.terms=TRUE, pch=16)
limn
summary(Limnothrixexperience)


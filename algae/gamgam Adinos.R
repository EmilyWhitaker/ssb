library(tidyverse)
library(lubridate)
library(ggplot2)
library(mgcv)

Adinosyears <- read_csv("Adino_lite3_chloro_datayearonly.csv", 
                       col_types = cols(X1 = col_skip(), X1.x = col_skip(), 
                                        daynum = col_skip(), sampledate = col_date(format = "%m/%d/%Y")))
View(Adinosyears)

##Add in daynumber        
Adinosyears$daynum<- yday(Adinosyears$sampledate)
Adinosyears

Adinosexperience <- gam(CellBioVol ~ s(year4.x, bs = "gp")+ s(daynum), data=Adinosyears)
Ad<-plot(Adinosexperience, page=1)
Ad<-plot(Adinosexperience,pages=1,residuals=TRUE,all.terms=TRUE, pch=16)
Ad
summary(Adinosexperience)

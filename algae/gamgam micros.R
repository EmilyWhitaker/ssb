library(tidyverse)
library(lubridate)
library(ggplot2)
library(mgcv)

Microcystisyears <- read_csv("Microcystis_lte_lite3_chloro_datayearonly.csv", 
                            col_types = cols(X1 = col_skip(), X1.x = col_skip(), 
                                             daynum = col_skip(), sampledate = col_date(format = "%m/%d/%Y")))

##Add in daynumber        
Microcystisyears$daynum<- yday(Microcystisyears$sampledate)
Microcystisyears
######daynumb

Microcystisexperience <- gam(CellBioVol ~ s(year4.x, bs = "gp")+ s(daynum), data=Microcystisyears)
mirco<-plot(Microcystisexperience, page=1)
mirco<- plot(Microcystisexperience,pages=1,residuals=TRUE,all.terms=TRUE, pch=16)

micro
summary(Microcystisexperience)


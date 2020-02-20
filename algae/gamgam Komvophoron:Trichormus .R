library(tidyverse)
library(lubridate)
library(ggplot2)
library(mgcv)

CfKTyears <- read_csv("CfKT_lte_lite3_chloro_datayearonly.csv", 
                        col_types = cols(X1 = col_skip(), X1.x = col_skip(), 
                                         daynum = col_skip(), sampledate = col_date(format = "%m/%d/%Y")))
View(CfKTyears)

##Add in daynumber        
CfKTyears$daynum<- yday(CfKTyears$sampledate)
CfKTyears

CfKTexperience <- gam(CellBioVol ~ s(year4.x, bs = "gp")+ s(daynum), data=CfKTyears)
cfkt<-plot(CfKTexperience, page=1)
summary(CfKTexperience)
Cf. Komvophoron / Trichormus
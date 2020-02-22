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
cfkt <- plot(CfKTexperience,pages=1,residuals=TRUE,all.terms=TRUE, pch=16)
cfkt
summary(CfKTexperience)


CfKTexperience2 <- gam(CellBioVol ~ s(year4.x, bs = "gp")+ s(daynum)+ s(CfKTyears$surfchlor), data=CfKTyears)
cfkt2<-plot(CfKTexperience, page=1)
cfkt2<- plot(CfKTexperience2,pages=1,residuals=TRUE,all.terms=TRUE, pch=16)
cfkt
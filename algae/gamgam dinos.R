library(tidyverse)
library(lubridate)
library(ggplot2)
library(mgcv)

dinosyears <- read_csv("Dinobryon_lte_lite3_chloro_datayearonly.csv", 
                       col_types = cols(X1 = col_skip(), X1.x = col_skip(), 
                                        daynum = col_skip(), sampledate = col_date(format = "%m/%d/%Y")))
View(dinosyears)
        
##Add in daynumber        
dinosyears$daynum<- yday(dinosyears$sampledate)
dinosyears
######daynumb
Dinobryonatzero2 <- read_csv('Dinobryonatzero.csv')
lte_lite <- read_csv('lte_lite.csv',
                     col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))

bothchloro<- read_csv('fullchloro.csv')


Dinobryon_lte_lite3 <- lte_lite %>%
  right_join(Dinobryonatzero2, by= c('sampledate'))
write.csv(Dinobryon_lte_lite3, 'Dinobryon_lte_lite3.csv')

Dinobryon_lte_lite3_chloro <-Dinobryon_lte_lite3 %>%
  right_join(bothchloro, by= c('sampledate'))
write.csv(Dinobryon_lte_lite3_chloro, 'Dinobryon_lte_lite3_chloro.csv')


dinosexperience <- gam(CellBioVol ~ s(year4.x, bs = "gp")+ s(daynum), data=dinosyears)
d<-plot(dinosexperience, page=1)
d<-plot(dinosexperience,pages=1,residuals=TRUE,all.terms=TRUE, pch=16)
d
summary(dinosexperience)

dinosexperience2 <- gam(CellBioVol ~ s(year4.x, bs = "gp")+ s(daynum)+ (surfchlor), data=dinosyears)
snow<- plot(dinosexperience2, page=1)
snow




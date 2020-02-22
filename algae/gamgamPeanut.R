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
c<- plot(Peanutexperience,pages=1,residuals=TRUE,all.terms=TRUE, pch=16)
c


ggsave(plot=c,filename='Peanutgams.png',height = 18, width =16, units = 'cm')
#confused why I can't save this 

          + geom_point(aes(Peanutyears$daynum, Peanutyears$CellBioVol)))
b




Peanutplot2<- plot(Peanutexperience)+ 
  geom_point(aes(x=daynum, y=CellBioVol))
summary(Peanutexperience)




fit_gam <- mydata %>% 
  gam(CellBioVol ~ s(exposure, k = 5) + covariate, data = .) %>% 
  predict(newdata = mydata, type = "link", se.fit = TRUE) %>% 
  as_data_frame() %>% 
  rename(fit_gam = fit) %>% 
  mutate(lwr_gam = fit_gam - 2 * se.fit,
         upr_gam = fit_gam + 2 * se.fit) %>% 
  select(-se.fit)




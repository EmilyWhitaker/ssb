library(tidyverse)
library(lubridate)
library(ggplot2)
library(mgcv)

dinosyears <- read_csv("Dinobryon_lte_lite3_chloro_datayearonly.csv", 
                       col_types = cols(X1 = col_skip(), X1.x = col_skip(),
                                        daynum = col_skip(), sampledate = col_date(format = "%m/%d/%Y")))
View(dinosyears)

dinosyears$daynum<- yday(dinosyears$sampledate)
dinosyears


dinosexperience <- gam(CellBioVol ~ s(year4.x, bs = "gp")+ s(daynum), data=dinosyears)
d<-plot(dinosexperience, page=1)
d<-plot(dinosexperience,pages=2,residuals=TRUE,all.terms=TRUE, pch=16, title= 'Yearly')
d
summary(dinosexperience)
gam.check(dinosexperience)

Dinos_season_time <-gam(CellBioVol ~ s(daynum, bs = 'cc')+ s(year4.x), data=dinosyears, method = "REML")
par(mfrow = c(1,2))
plot(Dinos_season_time)
d<-plot(Dinos_season_time,pages=1,residuals=TRUE,all.terms=TRUE, pch=16)
gam.check(Dinos_season_time)



Dinos_pred <- data.frame(daynum = dinosyears$daynum,
                       CellBV = dinosyears$CellBioVol,
                       predicted_values = predict(Dinos_season_time, newdata = dinosyears))
ggplot(Dinos_pred, aes(x = daynum)) +
  geom_point(aes(y = dinosyears$CellBioVol), size = 1, alpha = 0.5) + geom_line(aes(y = predicted_values), colour = "red")




CO2_season_time <- gam(co2 ~ s(month, bs = 'cc', k = 12) + s(time), data = CO2_dat, method = "REML")
par(mfrow = c(1,2))
plot(CO2_season_time)

CO2_pred <- data.frame(time = CO2_dat$time,
                       co2 = CO2_dat$co2,
                       predicted_values = predict(CO2_season_time, newdata = CO2_dat))
ggplot(CO2_pred, aes(x = time)) +
  geom_point(aes(y = co2), size = 1, alpha = 0.5) + geom_line(aes(y = predicted_values), colour = "red")

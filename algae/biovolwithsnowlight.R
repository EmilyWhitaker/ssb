#looking for realtionship bw bv and ice type/snow/par?

library(tidyverse)
library(lubridate)
library(ggplot2)

### using df lte

icesnow02<- read_csv("sparkling_icesnowo2par.csv") %>%
  filter(depth == '0')

# need blueice 
lte_blu<-
  icesnow02 %>%
  mutate(blueice = totice - whiteice)

# % surface light
lte_lite <-
  lte_blu %>%
  mutate(surflite = light / deck)

write.csv(lte_lite, 'lte_lite.csv')
#add in bv
all = read_csv('bvicesnowo2.csv')

## linear model

#fit = lm(surflite ~ avsnow + whiteice +totice+ blueice, data = lte_lite)

fit= lm(CellBioVol ~ avsnow + whiteice + blueice + surflite + o2, data= all )
summary(fit)

fit2= lm(CellBioVol ~ avsnow + totice+ surflite + o2, data= all)
summary(fit2)


#fit2 = lm(surflite ~ avsnow + whiteice + blueice, data = lte_lite)
slbv <- ggplot(data = all, mapping = aes(x = surflite, y = CellBioVol))+geom_point()
slbv

snbv <- ggplot(data = all, mapping = aes(x = avsnow, y = CellBioVol))+geom_line()
snbv


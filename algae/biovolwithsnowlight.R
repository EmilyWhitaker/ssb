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

#adding in another column/ multiple columns 
Limnothrixatzero2 <- read_csv('Limnothrixatzero2.csv',
                              col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))
lte_lite <- read_csv('lte_lite.csv',
                              col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))

Limnothrixlte_lite3 <- lte_lite %>%
  right_join(Limnothrixatzero2, by= c('sampledate'))
write.csv(Limnothrixlte_lite3, 'Limnothrixlte_lite3.csv')

lll3light <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=surflite))+geom_line()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "surf light")+
  theme_bw()
lll3light




Microcystisatzero2 <- read_csv('Microcystisatzero2.csv',
                               col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))
mzO<- ggplot(Microcystisatzero2, aes(x= CellBioVol, y=o2))+geom_line()+
  facet_wrap('group')+
  labs(x = "Biovolume Microcystis",
       y = "o2")+
  theme_bw()
mzO
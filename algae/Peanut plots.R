
library(tidyverse)
library(lubridate)
library(ggplot2)


Peanutatzero2 <- read_csv('Peanutatzero2.csv',
                               col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))
lte_lite <- read_csv('lte_lite.csv',
                     col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))

bothchloro<- read_csv('fullchloro.csv')


Peanutlte_lite3 <- lte_lite %>%
  right_join(Peanutatzero2, by= c('sampledate'))
write.csv(Peanutlte_lite3, 'Peanutlte_lite3.csv')

Peanutlte_lite3_chloro <-Peanutlte_lite3 %>%
  right_join(bothchloro, by= c('sampledate'))
write.csv(Peanutlte_lite3_chloro, 'Peanutlte_lite3_chloro.csv')

pll3light <- ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=surflite))+geom_line()+
  facet_wrap('group')+
  labs(x = "Biovolume Peanut",
       y = "surf light")+
  theme_bw()
pll3light

pll3snow <- ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=avsnow))+geom_point()+
  labs(x = "Biovolume Peanut",
       y = "av snow")+
  theme_bw()
pll3snow

pll3tice <- ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=totice))+geom_point()+
  labs(x = "Biovolume Peanut",
       y = "total ice")+
  theme_bw()
pll3tice

pll3whiteice <- ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=whiteice))+geom_point()+
  labs(x = "Biovolume Peanut",
       y = "white ice")+
  theme_bw()
pll3whiteice


pll3blueice <- ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=blueice))+geom_point()+
  labs(x = "Biovolume Peanut",
       y = "blue ice")+
  theme_bw()
pll3blueice

pll3surfchloro <-  ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=surfchlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Peanut",
       y = "surface chloro")+
  theme_bw()
pll3surfchloro

pll3chloro <-  ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=chlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Peanut",
       y = "int chloro")+
  theme_bw()
pll3chloro



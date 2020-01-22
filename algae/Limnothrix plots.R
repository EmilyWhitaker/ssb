library(tidyverse)
library(lubridate)
library(ggplot2)


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

lll3snow <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=avsnow))+geom_line()+
  labs(x = "Biovolume Limnothrix",
       y = "av snow")+
  theme_bw()
lll3snow

lll3tice <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=totice))+geom_line()+
  labs(x = "Biovolume Limnothrix",
       y = "total ice")+
  theme_bw()
lll3tice

lll3whiteice <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=whiteice))+geom_line()+
  labs(x = "Biovolume Limnothrix",
       y = "white ice")+
  theme_bw()
lll3whiteice


lll3blueice <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=blueice))+geom_line()+
  labs(x = "Biovolume Limnothrix",
       y = "blue ice")+
  theme_bw()
lll3blueice

lll3chloro

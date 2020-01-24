library(tidyverse)
library(lubridate)
library(ggplot2)


Microcystisatzero2 <- read_csv('Microcystisatzero2.csv',
                               col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))
lte_lite <- read_csv('lte_lite.csv',
                     col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))

bothchloro<- read_csv('fullchloro.csv')


Microcystislte_lite3 <- lte_lite %>%
  right_join(Microcystisatzero2, by= c('sampledate'))
write.csv(Microcystislte_lite3, 'Microcystislte_lite3.csv')

Microcystislte_lite3_chloro <-Microcystislte_lite3 %>%
  right_join(bothchloro, by= c('sampledate'))
write.csv(Microcystislte_lite3_chloro, 'Microcystislte_lite3_chloro.csv')

mll3light <- ggplot(Microcystislte_lite3, aes(x= CellBioVol, y=surflite))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Microcystis",
       y = "surf light")+
  theme_bw()
mll3light
ggsave(plot=mll3light,filename='mll3light.png',height = 18, width =16, units = 'in')

mll3snow <- ggplot(Microcystislte_lite3, aes(x= CellBioVol, y=avsnow))+geom_point()+
  labs(x = "Biovolume Microcystis",
       y = "av snow")+
  theme_bw()
mll3snow
ggsave(plot=mll3snow,filename='mll3snow.png',height = 18, width =16, units = 'in')


mll3tice <- ggplot(Microcystislte_lite3, aes(x= CellBioVol, y=totice))+geom_point()+
  labs(x = "Biovolume Microcystis",
       y = "total ice")+
  theme_bw()
mll3tice
ggsave(plot=mll3tice,filename='mll3tice.png',height = 18, width =16, units = 'in')

mll3whiteice <- ggplot(Microcystislte_lite3, aes(x= CellBioVol, y=whiteice))+geom_line()+
  labs(x = "Biovolume Microcystis",
       y = "white ice")+
  theme_bw()
mll3whiteice


mll3blueice <- ggplot(Microcystislte_lite3, aes(x= CellBioVol, y=blueice))+geom_line()+
  labs(x = "Biovolume Microcystis",
       y = "blue ice")+
  theme_bw()
mll3blueice

mll3surfchloro <-  ggplot(Microcystislte_lite3_chloro, aes(x= CellBioVol, y=surfchlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Microcystis",
       y = "surface chloro")+
  theme_bw()
mll3surfchloro

mll3chloro <-  ggplot(Microcystislte_lite3_chloro, aes(x= CellBioVol, y=chlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Microcystis",
       y = "int chloro")+
  theme_bw()
mll3chloro



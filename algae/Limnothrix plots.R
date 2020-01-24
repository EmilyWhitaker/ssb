library(tidyverse)
library(lubridate)
library(ggplot2)


Limnothrixatzero2 <- read_csv('Limnothrixatzero2.csv',
                              col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))
lte_lite <- read_csv('lte_lite.csv',
                     col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))

bothchloro<- read_csv('fullchloro.csv')


Limnothrixlte_lite3 <- lte_lite %>%
  right_join(Limnothrixatzero2, by= c('sampledate'))
write.csv(Limnothrixlte_lite3, 'Limnothrixlte_lite3.csv')

Limnothrixlte_lite3_chloro <-Limnothrixlte_lite3 %>%
  right_join(bothchloro, by= c('sampledate'))
write.csv(Limnothrixlte_lite3_chloro, 'Limnothrixlte_lite3_chloro.csv')

lll3light <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=surflite))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "surf light")+
  theme_bw()
lll3light
ggsave(plot=lll3light,filename='lll3light.png',height = 18, width =16, units = 'in')

lll3snow <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=avsnow))+ geom_point()+
  labs(x = "Biovolume Limnothrix",
       y = "av snow")+
  theme_bw()
lll3snow
ggsave(plot=lll3snow,filename='lll3snow.png',height = 18, width =16, units = 'in')


lll3tice <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=totice))+geom_point()+
  labs(x = "Biovolume Limnothrix",
       y = "total ice")+
  theme_bw()
lll3tice
ggsave(plot=lll3tice,filename='lll3tice.png',height = 18, width =16, units = 'in')

lll3whiteice <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=whiteice))+geom_point()+
  labs(x = "Biovolume Limnothrix",
       y = "white ice")+
  theme_bw()
lll3whiteice
ggsave(plot=lll3whiteice,filename='lll3whiteice.png',height = 18, width =16, units = 'in')


lll3blueice <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=blueice))+geom_point()+
  labs(x = "Biovolume Limnothrix",
       y = "blue ice")+
  theme_bw()
lll3blueice
ggsave(plot=lll3blueice,filename='lll3blueice.png',height = 18, width =16, units = 'in')

lll3surfchloro <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=surfchlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "surface chloro")+
  theme_bw()
lll3surfchloro
ggsave(plot=lll3surfchloro,filename='lll3surfchloro.png',height = 18, width =16, units = 'in')

lll3chloro <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=chlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "int chloro")+
  theme_bw()
lll3chloro
ggsave(plot=lll3chloro,filename='lll3chloro.png',height = 18, width =16, units = 'in')

lll3wtemp <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=wtemp))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "int wtemp")+
  theme_bw()
lll3wtemp
ggsave(plot=lll3wtemp,filename='Limnothrixintwtemp.png',height = 18, width =16, units = 'in')



library(tidyverse)
library(lubridate)
library(ggplot2)


NDzero2 <- read_csv('NakedDinoatzero.csv')
lte_lite <- read_csv('lte_lite.csv',
                     col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))

bothchloro<- read_csv('fullchloro.csv')


NDlte_lite3 <- lte_lite %>%
  right_join(NDzero2, by= c('sampledate'))
write.csv(NDlte_lite3, 'NDlte_lite3.csv')

NDlte_lite3_chloro <-NDlte_lite3 %>%
  right_join(bothchloro, by= c('sampledate'))
write.csv(NDlte_lite3_chloro, 'NDlte_lite3_chloro.csv')



ndl3light <- ggplot(NDlte_lite3_chloro, aes(x= CellBioVol, y=surflite))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume ND",
       y = "surf light")+
  theme_bw()
ndl3light
ggsave(plot=ndl3light,filename='NDsurflight.png',height = 18, width =16, units = 'in')


ndl3snow <- ggplot(NDlte_lite3_chloro, aes(x= CellBioVol, y=avsnow))+geom_point()+
  labs(x = "Biovolume ND",
       y = "av snow")+
  theme_bw()
ndl3snow
ggsave(plot=ndl3snow,filename='NDavsnow.png',height = 18, width =16, units = 'in')

ndl3tice <- ggplot(NDlte_lite3_chloro, aes(x= CellBioVol, y=totice))+geom_point()+
  labs(x = "Biovolume ND",
       y = "total ice")+
  theme_bw()
ndl3tice
ggsave(plot=ndl3tice,filename='NDtotalice.png',height = 18, width =16, units = 'in')

ndl3whiteice <- ggplot(NDlte_lite3_chloro, aes(x= CellBioVol, y=whiteice))+geom_point()+
  labs(x = "Biovolume Peanut",
       y = "white ice")+
  theme_bw()
ndl3whiteice
ggsave(plot=ndl3whiteice,filename='ndwhiteice.png',height = 18, width =16, units = 'in')


ndl3blueice <- ggplot(NDlte_lite3_chloro, aes(x= CellBioVol, y=blueice))+geom_point()+
  labs(x = "Biovolume ND",
       y = "blue ice")+
  theme_bw()
ndl3blueice
ggsave(plot=ndl3blueice,filename='ndblueice.png',height = 18, width =16, units = 'in')

ndl3surfchloro <-  ggplot(NDlte_lite3_chloro, aes(x= CellBioVol, y=surfchlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume ND",
       y = "surface chloro")+
  theme_bw()
ndl3surfchloro
ggsave(plot=ndl3surfchloro,filename='ndsurfchloro.png',height = 18, width =16, units = 'in')

ndl3wtemp <-  ggplot(NDlte_lite3_chloro, aes(x= CellBioVol, y=wtemp))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume ND",
       y = "int wtemp")+
  theme_bw()
ndl3wtemp
ggsave(plot=ndl3wtemp,filename='ndintwtemp.png',height = 18, width =16, units = 'in')


ndl3chloro <-  ggplot(NDlte_lite3_chloro, aes(x= CellBioVol, y=chlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume ND",
       y = "int chloro")+
  theme_bw()
ndl3chloro
ggsave(plot=ndl3chloro,filename='ndintchloro.png',height = 18, width =16, units = 'in')


ndl3o2 <-  ggplot(NDlte_lite3_chloro, aes(x= CellBioVol, y=o2))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume ND",
       y = "int o2")+
  theme_bw()
ndl3o2
ggsave(plot=ndl3o2,filename='ndinto2.png',height = 18, width =16, units = 'in')

ndl3o2sat <-  ggplot(NDlte_lite3_chloro, aes(x= CellBioVol, y=o2sat))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume ND",
       y = "int o2 sat")+
  theme_bw()
ndl3o2sat
ggsave(plot=ndl3o2sat,filename='NDinto2sat.png',height = 18, width =16, units = 'in')

ndl3cond <-  ggplot(NDlte_lite3_chloro, aes(x= CellBioVol, y=cond))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume ND",
       y = "int cond")+
  theme_bw()
ndl3cond
ggsave(plot=ndl3cond,filename='ndintcond.png',height = 18, width =16, units = 'in')

ndl3doc.y <-  ggplot(NDlte_lite3_chloro, aes(x= CellBioVol, y=doc.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume ND",
       y = "int doc")+
  theme_bw()
ndl3doc.y
ggsave(plot=ndl3doc.y,filename='ndintdoc.png',height = 18, width =16, units = 'in')

ndl3ph.y <-  ggplot(NDlte_lite3_chloro, aes(x= CellBioVol, y=ph.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume ND",
       y = "int ph")+
  theme_bw()
ndl3ph.y
ggsave(plot=ndl3ph.y,filename='ndintph.png',height = 18, width =16, units = 'in')


#### tool to graph all te graphs next to eahother~~~~ and then want like types of graphs across 

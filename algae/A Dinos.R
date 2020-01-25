library(tidyverse)
library(lubridate)
library(ggplot2)

#method to color max point through out? or date?

Adinoatzerozero2 <- read_csv('Adinoatzero.csv')
lte_lite <- read_csv('lte_lite.csv',
                     col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))

bothchloro<- read_csv('fullchloro.csv')


Adino_lte_lite3 <- lte_lite %>%
  right_join(Adinoatzerozero2, by= c('sampledate'))
write.csv(Adino_lte_lite3, 'Adino_lte_lite3.csv')

Adino_lite3_chloro <-Adino_lte_lite3 %>%
  right_join(bothchloro, by= c('sampledate'))
write.csv(Adino_lite3_chloro, 'Adino_lite3_chloro.csv')


adl3light <- ggplot(Adino_lite3_chloro, aes(x= CellBioVol, y=surflite))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume AD",
       y = "surf light")+
  theme_bw()
adl3light
ggsave(plot=adl3light,filename='ADsurflight.png',height = 18, width =16, units = 'in')


adl3snow <- ggplot(Adino_lite3_chloro, aes(x= CellBioVol, y=avsnow))+geom_point()+
  labs(x = "Biovolume AD",
       y = "av snow")+
  theme_bw()
adl3snow
ggsave(plot=adl3snow,filename='ADavsnow.png',height = 18, width =16, units = 'in')

adl3tice <- ggplot(Adino_lite3_chloro, aes(x= CellBioVol, y=totice))+geom_point()+
  labs(x = "Biovolume AD",
       y = "total ice")+
  theme_bw()
adl3tice
ggsave(plot=adl3tice,filename='ADtotalice.png',height = 18, width =16, units = 'in')

adl3whiteice <- ggplot(Adino_lite3_chloro, aes(x= CellBioVol, y=whiteice))+geom_point()+
  labs(x = "Biovolume AD",
       y = "white ice")+
  theme_bw()
adl3whiteice
ggsave(plot=adl3whiteice,filename='adwhiteice.png',height = 18, width =16, units = 'in')


adl3blueice <- ggplot(Adino_lite3_chloro, aes(x= CellBioVol, y=blueice))+geom_point()+
  labs(x = "Biovolume AD",
       y = "blue ice")+
  theme_bw()
adl3blueice
ggsave(plot=adl3blueice,filename='adblueice.png',height = 18, width =16, units = 'in')

adl3surfchloro <-  ggplot(Adino_lite3_chloro, aes(x= CellBioVol, y=surfchlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume AD",
       y = "surface chloro")+
  theme_bw()
adl3surfchloro
ggsave(plot=adl3surfchloro,filename='adsurfchloro.png',height = 18, width =16, units = 'in')

adl3wtemp <-  ggplot(Adino_lite3_chloro, aes(x= CellBioVol, y=wtemp))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume AD",
       y = "int wtemp")+
  theme_bw()
adl3wtemp
ggsave(plot=adl3wtemp,filename='adintwtemp.png',height = 18, width =16, units = 'in')


adl3chloro <-  ggplot(Adino_lite3_chloro, aes(x= CellBioVol, y=chlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume AD",
       y = "int chloro")+
  theme_bw()
adl3chloro
ggsave(plot=adl3chloro,filename='adintchloro.png',height = 18, width =16, units = 'in')


adl3o2 <-  ggplot(Adino_lite3_chloro, aes(x= CellBioVol, y=o2))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume AD",
       y = "int o2")+
  theme_bw()
adl3o2
ggsave(plot=adl3o2,filename='adinto2.png',height = 18, width =16, units = 'in')

adl3o2sat <-  ggplot(Adino_lite3_chloro, aes(x= CellBioVol, y=o2sat))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume AD",
       y = "int o2 sat")+
  theme_bw()
adl3o2sat
ggsave(plot=adl3o2sat,filename='ADinto2sat.png',height = 18, width =16, units = 'in')

adl3cond <-  ggplot(Adino_lite3_chloro, aes(x= CellBioVol, y=cond))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume AD",
       y = "int cond")+
  theme_bw()
adl3cond
ggsave(plot=adl3cond,filename='adintcond.png',height = 18, width =16, units = 'in')

adl3doc.y <-  ggplot(Adino_lite3_chloro, aes(x= CellBioVol, y=doc.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume AD",
       y = "int doc")+
  theme_bw()
adl3doc.y
ggsave(plot=adl3doc.y,filename='adintdoc.png',height = 18, width =16, units = 'in')

adl3ph.y <-  ggplot(Adino_lite3_chloro, aes(x= CellBioVol, y=ph.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume AD",
       y = "int ph")+
  theme_bw()
adl3ph.y
ggsave(plot=adl3ph.y,filename='adintph.png',height = 18, width =16, units = 'in')


#### tool to graph all the graphs next to eahother~~~~ and then want like types of graphs across 

library("cowplot")
adl3_all <- plot_grid( adl3snow, adl3tice, adl3whiteice, adl3blueice, adl3ph.y, adl3doc.y, adl3light, adl3surfchloro,
                       adl3chloro, adl3wtemp, adl3o2,adl3o2sat,
                       labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
                       ncol = 4, nrow = 3)
ggsave(plot=adl3_all,filename='adl3_all.png',height = 40, width =49, units = 'in')
adl3_all

library(tidyverse)
library(lubridate)
library(ggplot2)

#method to color max point through out? or date?

Dinobryon_lte_lite3_chloro_datayearonly <- read_csv('Dinobryon_lte_lite3_chloro_datayearonly.csv')

Dinobryonatzero2 <- read_csv('Dinobryonatzero.csv')
lte_lite <- read_csv('lte_lite.csv',
                     col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))

bothchloro<- read_csv('fullchloro.csv')


Dinobryon_lte_lite3 <- lte_lite %>%
  right_join(Dinobryonatzero2, by= c('sampledate'))
write.csv(Dinobryon_lte_lite3, 'Dinobryon_lte_lite3.csv')

Dinobryon_lte_lite3_chloro <-Dinobryon_lte_lite3 %>%
  right_join(bothchloro, by= c('sampledate'))
write.csv(Dinobryon_lte_lite3_chloro, 'Dinobryon_lte_lite3_chloro.csv')


dbl3light <- ggplot(Dinobryon_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=surflite))+geom_point(size=4)+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "surf light")+
  theme_bw()
dbl3light
ggsave(plot=dbl3light,filename='DBsurflight.png',height = 18, width =16, units = 'in')
dbl3lightyear<- dbl3light+ geom_point(aes(color = factor(year4.x)))
ggsave(plot=dbl3lightyear,filename='DBsurflightyear.png',height = 18, width =16, units = 'in')
dbl3lightycholor<- dbl3light+ geom_point(aes(color = factor(chlor)))
dbl3lightycholor
ggsave(plot=dbl3lightycholor,filename='DBsurflightchloro.png',height = 18, width =16, units = 'in')


dbl3snow2 <- ggplot(Dinobryon_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=avsnow))+geom_point(size=4)+
  labs(x = "Biovolume Dinobryon",
       y = "av snow")+
  theme_bw()
dbl3snow2
ggsave(plot=dbl3snow2,filename='DBavsnow2.png',height = 18, width =16, units = 'in')

dbl3snowyear<- dbl3snow2+ geom_point(aes(color = factor(year4.x)))
dbl3snowyear+ xlim(0, 15000)
ggsave(plot=dbl3snowyear,filename='DBsnowyear2.png',height = 18, width =16, units = 'in')



dbl3tice <- ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=totice))+geom_point()+
  labs(x = "Biovolume Dinobryon",
       y = "total ice")+
  theme_bw()
dbl3tice
ggsave(plot=dbl3tice,filename='DBtotalice.png',height = 18, width =16, units = 'in')

dbl3tice <- ggplot(Dinobryon_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=totice))+geom_point()+
  labs(x = "Biovolume Dinobryon",
       y = "total ice")+
  theme_bw()
dbl3tice
ggsave(plot=dbl3tice,filename='DBtotalice.png',height = 18, width =16, units = 'in')


dbl3whiteice <- ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=whiteice))+geom_point()+
  labs(x = "Biovolume Dinobryon",
       y = "white ice")+
  theme_bw()
dbl3whiteice
ggsave(plot=dbl3whiteice,filename='DBwhiteice.png',height = 18, width =16, units = 'in')

dbl3whiteice2 <- ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=whiteice))+geom_point()+
  labs(x = "Biovolume Dinobryon",
       y = "white ice")+
  theme_bw()
dbl3whiteice2
ggsave(plot=dbl3whiteice2,filename='DBwhiteice.png',height = 18, width =16, units = 'in')


dbl3blueice <- ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=blueice))+geom_point()+
  labs(x = "Biovolume Dinobryon",
       y = "blue ice")+
  theme_bw()
dbl3blueice
ggsave(plot=dbl3blueice,filename='DBlueice.png',height = 18, width =16, units = 'in')

dbl3surfchloro <-  ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=surfchlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "surface chloro")+
  theme_bw()
dbl3surfchloro
ggsave(plot=dbl3surfchloro,filename='DBsurfchloro.png',height = 18, width =16, units = 'in')

dbl3wtemp <-  ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=wtemp))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int wtemp")+
  theme_bw()
dbl3wtemp
ggsave(plot=dbl3wtemp,filename='DBintwtemp.png',height = 18, width =16, units = 'in')


dbl3chloro <-  ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=chlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int chloro")+
  theme_bw()
dbl3chloro
ggsave(plot=dbl3chloro,filename='DBintchloro.png',height = 18, width =16, units = 'in')


dbl3o2 <-  ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=o2))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int o2")+
  theme_bw()
dbl3o2
ggsave(plot=dbl3o2,filename='DBinto2.png',height = 18, width =16, units = 'in')

dbl3o2sat <-  ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=o2sat))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int o2 sat")+
  theme_bw()
dbl3o2sat
ggsave(plot=dbl3o2sat,filename='DBinto2sat.png',height = 18, width =16, units = 'in')

dbl3cond <-  ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=cond))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int cond")+
  theme_bw()
dbl3cond
ggsave(plot=dbl3cond,filename='DBintcond.png',height = 18, width =16, units = 'in')

dbl3doc.y <-  ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=doc.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int doc")+
  theme_bw()
dbl3doc.y
ggsave(plot=dbl3doc.y,filename='DBintdoc.png',height = 18, width =16, units = 'in')

dbl3ph.y <-  ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=ph.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int ph")+
  theme_bw()
dbl3ph.y
ggsave(plot=dbl3ph.y,filename='DBintph.png',height = 18, width =16, units = 'in')


#### tool to graph all the graphs next to eahother~~~~ and then want like types of graphs across 

library("cowplot")
dbl3_all <- plot_grid( dbl3snow, dbl3tice, dbl3whiteice, dbl3blueice, dbl3ph.y, dbl3doc.y, dbl3light, dbl3surfchloro,
                       dbl3chloro, dbl3wtemp, dbl3o2,dbl3o2sat,
                       labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
                       ncol = 4, nrow = 3)
ggsave(plot=dbl3_all,filename='dbl3_all.png',height = 40, width =49, units = 'in')
dbl3_all


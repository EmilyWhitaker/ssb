library(tidyverse)
library(lubridate)
library(ggplot2)

#method to color max point through out? or date?

SMCOatzero2 <- read_csv('smcoatzero.csv')
lte_lite <- read_csv('lte_lite.csv',
                     col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))

bothchloro<- read_csv('fullchloro.csv')


SMCO_lte_lite3 <- lte_lite %>%
  right_join(SMCOatzero2, by= c('sampledate'))
write.csv(SMCO_lte_lite3, 'SMCO_lte_lite3.csv')

SMCO_lte_lite3_chloro <-SMCO_lte_lite3 %>%
  right_join(bothchloro, by= c('sampledate'))
write.csv(SMCO_lte_lite3_chloro, 'SMCO_lte_lite3_chloro.csv')


SMCOl3light <- ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=surflite))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume SMCO",
       y = "surf light")+
  theme_bw()
SMCOl3light
ggsave(plot=SMCOl3light,filename='SMCOsurflight.png',height = 18, width =16, units = 'in')


SMCOl3snow <- ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=avsnow))+geom_point()+
  labs(x = "Biovolume SMCO",
       y = "av snow")+
  theme_bw()
SMCOl3snow
ggsave(plot=SMCOl3snow,filename='SMCOavsnow.png',height = 18, width =16, units = 'in')

SMCOl3tice <- ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=totice))+geom_point()+
  labs(x = "Biovolume SMCO",
       y = "total ice")+
  theme_bw()
SMCOl3tice
ggsave(plot=SMCOl3tice,filename='SMCOtotalice.png',height = 18, width =16, units = 'in')

SMCOl3whiteice <- ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=whiteice))+geom_point()+
  labs(x = "Biovolume SMCO",
       y = "white ice")+
  theme_bw()
SMCOl3whiteice
ggsave(plot=SMCOl3whiteice,filename='SMCOwhiteice.png',height = 18, width =16, units = 'in')


SMCOl3blueice <- ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=blueice))+geom_point()+
  labs(x = "Biovolume SMCO",
       y = "blue ice")+
  theme_bw()
SMCOl3blueice
ggsave(plot=SMCOl3blueice,filename='SMCOblueice.png',height = 18, width =16, units = 'in')

SMCOl3surfchloro <-  ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=surfchlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume SMCO",
       y = "surface chloro")+
  theme_bw()
SMCOl3surfchloro
ggsave(plot=SMCOl3surfchloro,filename='SMCOsurfchloro.png',height = 18, width =16, units = 'in')

SMCOl3wtemp <-  ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=wtemp))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume SMCO",
       y = "int wtemp")+
  theme_bw()
SMCOl3wtemp
ggsave(plot=SMCOl3wtemp,filename='SMCOintwtemp.png',height = 18, width =16, units = 'in')


SMCOl3chloro <-  ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=chlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume SMCO",
       y = "int chloro")+
  theme_bw()
SMCOl3chloro
ggsave(plot=SMCOl3chloro,filename='SMCOintchloro.png',height = 18, width =16, units = 'in')


SMCOl3o2 <-  ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=o2))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume SMCO",
       y = "int o2")+
  theme_bw()
SMCOl3o2
ggsave(plot=SMCOl3o2,filename='SMCOinto2.png',height = 18, width =16, units = 'in')

CfKTl3o2sat <-  ggplot(CfKT_lte_lite3_chloro, aes(x= CellBioVol, y=o2sat))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume CfKT",
       y = "int o2 sat")+
  theme_bw()
CfKTl3o2sat
ggsave(plot=CfKTl3o2sat,filename='CfKTinto2sat.png',height = 18, width =16, units = 'in')

CfKTl3cond <-  ggplot(CfKT_lte_lite3_chloro, aes(x= CellBioVol, y=cond))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume CfKT",
       y = "int cond")+
  theme_bw()
CfKTl3cond
ggsave(plot=CfKTl3cond,filename='CfKTintcond.png',height = 18, width =16, units = 'in')

CfKTl3doc.y <-  ggplot(CfKT_lte_lite3_chloro, aes(x= CellBioVol, y=doc.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume CfKT",
       y = "int doc")+
  theme_bw()
CfKTl3doc.y
ggsave(plot=CfKTl3doc.y,filename='CfKTintdoc.png',height = 18, width =16, units = 'in')

CfKTl3ph.y <-  ggplot(CfKT_lte_lite3_chloro, aes(x= CellBioVol, y=ph.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume CfKT",
       y = "int ph")+
  theme_bw()
CfKTl3ph.y
ggsave(plot=CfKTl3ph.y,filename='CfKTintph.png',height = 18, width =16, units = 'in')


#### tool to graph all te graphs next to eahother~~~~ and then want like types of graphs across 




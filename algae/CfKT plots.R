library(tidyverse)
library(lubridate)
library(ggplot2)

#method to color max point through out? or date?

CfKTatzero2 <- read_csv('CfKTatzero.csv')
lte_lite <- read_csv('lte_lite.csv',
                     col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))

bothchloro<- read_csv('fullchloro.csv')


CfKT_lte_lite3 <- lte_lite %>%
  right_join(CfKTatzero2, by= c('sampledate'))
write.csv(CfKT_lte_lite3, 'CfKT_lte_lite3.csv')

CfKT_lte_lite3_chloro <-CfKT_lte_lite3 %>%
  right_join(bothchloro, by= c('sampledate'))
write.csv(CfKT_lte_lite3_chloro, 'CfKT_lte_lite3_chloro.csv')


CfKTl3light <- ggplot(CfKT_lte_lite3_chloro, aes(x= CellBioVol, y=surflite))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume CfKT",
       y = "surf light")+
  theme_bw()
CfKTl3light
ggsave(plot=CfKTl3light,filename='CfKTsurflight.png',height = 18, width =16, units = 'in')


CfKTl3snow <- ggplot(CfKT_lte_lite3_chloro, aes(x= CellBioVol, y=avsnow))+geom_point()+
  labs(x = "Biovolume CfKT",
       y = "av snow")+
  theme_bw()
CfKTl3snow
ggsave(plot=CfKTl3snow,filename='CfKTavsnow.png',height = 18, width =16, units = 'in')

CfKTl3tice <- ggplot(CfKT_lte_lite3_chloro, aes(x= CellBioVol, y=totice))+geom_point()+
  labs(x = "Biovolume CfKT",
       y = "total ice")+
  theme_bw()
CfKTl3tice
ggsave(plot=CfKTl3tice,filename='CfKTtotalice.png',height = 18, width =16, units = 'in')

CfKTl3whiteice <- ggplot(CfKT_lte_lite3_chloro, aes(x= CellBioVol, y=whiteice))+geom_point()+
  labs(x = "Biovolume CfKT",
       y = "white ice")+
  theme_bw()
CfKTl3whiteice
ggsave(plot=CfKTl3whiteice,filename='CfKTwhiteice.png',height = 18, width =16, units = 'in')


CfKTl3blueice <- ggplot(CfKT_lte_lite3_chloro, aes(x= CellBioVol, y=blueice))+geom_point()+
  labs(x = "Biovolume CfKT",
       y = "blue ice")+
  theme_bw()
CfKTl3blueice
ggsave(plot=CfKTl3blueice,filename='CfKTlueice.png',height = 18, width =16, units = 'in')

CfKTl3surfchloro <-  ggplot(CfKT_lte_lite3_chloro, aes(x= CellBioVol, y=surfchlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume CfKT",
       y = "surface chloro")+
  theme_bw()
CfKTl3surfchloro
ggsave(plot=CfKTl3surfchloro,filename='CfKTsurfchloro.png',height = 18, width =16, units = 'in')

CfKTl3wtemp <-  ggplot(CfKT_lte_lite3_chloro, aes(x= CellBioVol, y=wtemp))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume CfKT",
       y = "int wtemp")+
  theme_bw()
CfKTl3wtemp
ggsave(plot=CfKTl3wtemp,filename='CfKTintwtemp.png',height = 18, width =16, units = 'in')


CfKTl3chloro <-  ggplot(CfKT_lte_lite3_chloro, aes(x= CellBioVol, y=chlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume CfKT",
       y = "int chloro")+
  theme_bw()
CfKTl3chloro
ggsave(plot=CfKTl3chloro,filename='CfKTintchloro.png',height = 18, width =16, units = 'in')


CfKTl3o2 <-  ggplot(CfKT_lte_lite3_chloro, aes(x= CellBioVol, y=o2))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume CfKT",
       y = "int o2")+
  theme_bw()
CfKTl3o2
ggsave(plot=CfKTl3o2,filename='CfKTinto2.png',height = 18, width =16, units = 'in')

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




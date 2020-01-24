library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)

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

SMCOl3o2sat <-  ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=o2sat))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume SMCO",
       y = "int o2 sat")+
  theme_bw()
SMCOl3o2sat
ggsave(plot=SMCOl3o2sat,filename='SMCOinto2sat.png',height = 18, width =16, units = 'in')

SMCOl3cond <-  ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=cond))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume SMCO",
       y = "int cond")+
  theme_bw()
SMCOl3cond
ggsave(plot=SMCOl3cond,filename='SMCOintcond.png',height = 18, width =16, units = 'in')

SMCOl3doc.y <-  ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=doc.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume SMCO",
       y = "int doc")+
  theme_bw()
SMCOl3doc.y
ggsave(plot=SMCOl3doc.y,filename='SMCOintdoc.png',height = 18, width =16, units = 'in')

SMCOl3ph.y <-  ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=ph.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume SMCO",
       y = "int ph")+
  theme_bw()
SMCOl3ph.y
ggsave(plot=SMCOl3ph.y,filename='SMCOintph.png',height = 18, width =16, units = 'in')

SMCOl3chlorobio <-  ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=chlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume SMCO",
       y = "int chlor")+
  theme_bw()
SMCOl3chlorobio
ggsave(plot=SMCOl3chlorobio,filename='SMCOintchlorobio.png',height = 18, width =16, units = 'in')

SMCOl3surfchlorobio <-  ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=surfchlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume SMCO",
       y = "surf chlor")+
  theme_bw()
SMCOl3surfchlorobio
ggsave(plot=SMCOl3surfchlorobio,filename='SMCOsurfchlorobio.png',height = 18, width =16, units = 'in')

#### tool to graph all te graphs next to eahother~~~~ and then want like types of graphs across 

grid.arrange(SMCOl3ph.y, SMCOl3doc.y, nrow = 1)

gg.arrange(SMCOl3ph.y, SMCOl3doc.y, SMCOl3light + rremove("x.text"), 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)

library("cowplot")
SMOC_all <- plot_grid( SMCOl3snow, SMCOl3tice, SMCOl3whiteice, SMCOl3blueice, SMCOl3ph.y, SMCOl3doc.y, SMCOl3light,SMCOl3surfchloro,
          SMCOl3chloro, SMCOl3wtemp, SMCOl3o2, SMCOl3o2sat,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
          ncol = 4, nrow = 3)
ggsave(plot=SMOC_all,filename='SMOC_all.png',height = 40, width =49, units = 'in')

Snow_All <-plot_grid(lll3snow, mll3snow, pll3snow, ndl3snow, adl3snow, dbl3snow, CfKTl3snow, SMCOl3snow,
                      labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                      ncol = 3, nrow = 3)
Snow_All
ggsave(plot=Snow_All,filename='SSnow_All.png',height = 40, width =49, units = 'in')
  

  
### awk stats###
cor.test(SMCO_lte_lite3_chloro$CellBioVol, SMCO_lte_lite3_chloro$surfchlor) #not
cor.test(SMCO_lte_lite3_chloro$CellBioVol, SMCO_lte_lite3_chloro$surflite) #pvalue, .09481


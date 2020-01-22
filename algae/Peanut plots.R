
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
ggsave(plot=pll3light,filename='Peanutsurflight.png',height = 18, width =16, units = 'in')


pll3snow <- ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=avsnow))+geom_point()+
  labs(x = "Biovolume Peanut",
       y = "av snow")+
  theme_bw()
pll3snow
ggsave(plot=pll3snow,filename='Peanutavsnow.png',height = 18, width =16, units = 'in')

pll3tice <- ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=totice))+geom_point()+
  labs(x = "Biovolume Peanut",
       y = "total ice")+
  theme_bw()
pll3tice
ggsave(plot=pll3tice,filename='Peanuttotalice.png',height = 18, width =16, units = 'in')

pll3whiteice <- ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=whiteice))+geom_point()+
  labs(x = "Biovolume Peanut",
       y = "white ice")+
  theme_bw()
pll3whiteice
ggsave(plot=pll3whiteice,filename='Peanutwhiteice.png',height = 18, width =16, units = 'in')


pll3blueice <- ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=blueice))+geom_point()+
  labs(x = "Biovolume Peanut",
       y = "blue ice")+
  theme_bw()
pll3blueice
ggsave(plot=pll3blueice,filename='Peanutblueice.png',height = 18, width =16, units = 'in')

pll3surfchloro <-  ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=surfchlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Peanut",
       y = "surface chloro")+
  theme_bw()
pll3surfchloro
ggsave(plot=pll3surfchloro,filename='Peanutsurfchloro.png',height = 18, width =16, units = 'in')

pll3wtemp <-  ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=wtemp))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Peanut",
       y = "int wtemp")+
  theme_bw()
pll3wtemp
ggsave(plot=pll3wtemp,filename='Peanutintwtemp.png',height = 18, width =16, units = 'in')


pll3chloro <-  ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=chlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Peanut",
       y = "int chloro")+
  theme_bw()
pll3chloro
ggsave(plot=pll3chloro,filename='Peanutintchloro.png',height = 18, width =16, units = 'in')


pll3o2 <-  ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=o2))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Peanut",
       y = "int o2")+
  theme_bw()
pll3o2

pll3o2sat <-  ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=o2sat))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Peanut",
       y = "int o2 sat")+
  theme_bw()
pll3o2sat

pll3cond <-  ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=cond))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Peanut",
       y = "int cond")+
  theme_bw()
pll3cond

pll3doc.y <-  ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=doc.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Peanut",
       y = "int doc")+
  theme_bw()
pll3doc.y

pll3ph.y <-  ggplot(Peanutlte_lite3_chloro, aes(x= CellBioVol, y=ph.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Peanut",
       y = "int ph")+
  theme_bw()
pll3ph.y




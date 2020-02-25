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

Limnothrix_lte_lite3_chloro_datayearonly <- read_csv('Limnothrixlte_lite3_chloro_datayearonly.csv',
                                                     col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))



lll3light <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=surflite))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "surf light")+
  theme_bw()
lll3light
ggsave(plot=lll3light,filename='lll3light.png',height = 18, width =16, units = 'in')

lll3light <- ggplot(Limnothrix_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=surflite))+geom_point(size=2)+
  facet_wrap('group')+
  labs(x = "Limnothrix Biovolume",
       y = "surf light")+
  ggtitle('Seasonal Limnothrix Relationship with Surface Light')+
  theme_bw()
lll3light
ggsave(plot=lll3light,filename='LLsurflight.png',height = 18, width =16, units = 'cm')
lll3lightyear<- lll3light+ xlab('Limnothrix Biovolume (um3)') + ylab('Surface Light (units)')+ ylim(0,1)+
  geom_point(aes(color = factor(year4.x)))
lll3lightyear
ggsave(plot=lll3lightyear,filename='LLsurflightyear.png',height = 18, width =16, units = 'cm')
###needs work##


lll3snow <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=avsnow))+ geom_point()+
  labs(x = "Biovolume Limnothrix",
       y = "av snow")+
  theme_bw()
lll3snow
ggsave(plot=lll3snow,filename='lll3snow.png',height = 18, width =16, units = 'in')


lll3snow <- ggplot(Limnothrix_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=avsnow))+geom_point(size=2)+
  labs(x = "Limnothrix Biovolume",
       y = "av snow")+
  ggtitle('Seasonal Limnothrix Relationship with Average Snow')+
  theme_bw()
lll3snow
ggsave(plot=lll3snow,filename='LLsnow.png',height = 18, width =16, units = 'cm')
lll3snowyear<- lll3snow+ xlab('Limnothrix Biovolume (um3)') + ylab('Average Snow (cm)')+ 
  geom_point(aes(color = factor(year4.x)))
lll3snowyear
ggsave(plot=lll3snowyear,filename='LLsnowyear.png',height = 18, width =16, units = 'cm')


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

lll3o2 <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=o2))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "int o2")+
  theme_bw()
lll3o2
ggsave(plot=lll3o2,filename='Limnothrixso2.png',height = 18, width =16, units = 'in')

lll3o2sat <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=o2sat))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "int o2 sat")+
  theme_bw()
lll3o2sat
ggsave(plot=lll3o2sat,filename='Limnothrixinto2sat.png',height = 18, width =16, units = 'in')

lll3cond <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=cond))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "int cond")+
  theme_bw()
lll3cond
ggsave(plot= lll3cond,filename='Limnothrixintcond.png',height = 18, width =16, units = 'in')

lll3doc.y <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=doc.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "int doc")+
  theme_bw()
lll3doc.y
ggsave(plot=lll3doc.y,filename='Limnothrixintdoc.png',height = 18, width =16, units = 'in')

lll3ph.y <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=ph.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "int ph")+
  theme_bw()
lll3ph.y
ggsave(plot=lll3ph.y,filename='Limnothrixintph.png',height = 18, width =16, units = 'in')


library("cowplot")
lll3_all <- plot_grid( lll3snow, lll3tice, lll3whiteice, lll3blueice, lll3ph.y, lll3doc.y, lll3light,lll3surfchloro,
                       lll3chloro, lll3wtemp, lll3o2,lll3o2sat,
                       labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
                       ncol = 4, nrow = 3)
ggsave(plot=lll3_all,filename='lll3_all.png',height = 40, width =49, units = 'in')
lll3_all



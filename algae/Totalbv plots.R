library(tidyverse)
library(lubridate)
library(ggplot2)

#method to color max point through out? or date?
Totals<- read_csv("biovolume1.csv", col_types = cols(sampledate = col_date(format = "%m/%d/%Y"))) %>%
  filter(Genus == 'TotalBiovolume')
write.csv(Totals, 'TotalBVs.csv')

lte_lite <- read_csv('lte_lite.csv',
                     col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))

bothchloro<- read_csv('fullchloro.csv')

Totalszeros1<- Totals %>%
  left_join(zeroschem02, by= c('sampledate'))
write.csv(Totalszeros1, "Totalszeros1.csv")

Totals_lte_lite3 <- lte_lite %>%
  right_join(Totalszeros1, by= c('sampledate'))
write.csv(Totals_lte_lite3, 'Totals_lte_lite3.csv')

Totals_lte_lite3_chloro <-Totals_lte_lite3 %>%
  right_join(bothchloro, by= c('sampledate'))
write.csv(Totals_lte_lite3_chloro, 'Totals_lte_lite3_chloro.csv')



Totalsl3light <- ggplot(Totals_lte_lite3_chloro, aes(x= CellBioVol, y=surflite))+geom_point()+
  facet_wrap('group')+
  labs(x = "Total Biovolume",
       y = "surf light")+
  theme_bw()
Totalsl3light
ggsave(plot=Totalsl3light,filename='Totalsurflight.png',height = 18, width =16, units = 'in')

Totalsl3snow <- ggplot(Totals_lte_lite3_chloro, aes(x= CellBioVol, y=avsnow))+geom_point()+
  labs(x = "Total Biovolume",
       y = "av snow")+
  theme_bw()
Totalsl3snow
ggsave(plot=Totalsl3snow,filename='Totalsl3avsnow.png',height = 18, width =16, units = 'in')

Totalsl3tice <- ggplot(Totals_lte_lite3_chloro, aes(x= CellBioVol, y=totice))+geom_point()+
  labs(x = "Total Biovolume",
       y = "total ice")+
  theme_bw()
Totalsl3tice
ggsave(plot=Totalsl3tice,filename='Totalsl3totalice.png',height = 18, width =16, units = 'in')

Totalsl3whiteice <- ggplot(Totals_lte_lite3_chloro, aes(x= CellBioVol, y=whiteice))+geom_point()+
  labs(x = "Total Biovolume",
       y = "white ice")+
  theme_bw()
Totalsl3whiteice
ggsave(plot=Totalsl3whiteice,filename='Totalsl3whiteice.png',height = 18, width =16, units = 'in')


Totalsl3blueice <- ggplot(Totals_lte_lite3_chloro, aes(x= CellBioVol, y=blueice))+geom_point()+
  labs(x = "Total Biovolume",
       y = "blue ice")+
  theme_bw()
Totalsl3blueice
ggsave(plot=Totalsl3blueice,filename='Totalsl3lueice.png',height = 18, width =16, units = 'in')

Totalsl3surfchloro <-  ggplot(Totals_lte_lite3_chloro, aes(x= CellBioVol, y=surfchlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Total Biovolume",
       y = "surface chloro")+
  theme_bw()
Totalsl3surfchloro
ggsave(plot=Totalsl3surfchloro,filename='Totalsl3surfchloro.png',height = 18, width =16, units = 'in')

Totalsl3wtemp <-  ggplot(Totals_lte_lite3_chloro, aes(x= CellBioVol, y=wtemp))+geom_point()+
  facet_wrap('group')+
  labs(x = "Total Biovolume",
       y = "int wtemp")+
  theme_bw()
Totalsl3wtemp
ggsave(plot=Totalsl3wtemp,filename= 'Totalsl3intwtemp.png',height = 18, width =16, units = 'in')


Totalsl3chloro <-  ggplot(Totals_lte_lite3_chloro, aes(x= CellBioVol, y=chlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Total Biovolume",
       y = "int chloro")+
  theme_bw()
Totalsl3chloro
ggsave(plot=Totalsl3chloro,filename='Totalsl3intchloro.png',height = 18, width =16, units = 'in')


Totalsl3o2 <-  ggplot(Totals_lte_lite3_chloro, aes(x= CellBioVol, y=o2))+geom_point()+
  facet_wrap('group')+
  labs(x = "Total Biovolume",
       y = "int o2")+
  theme_bw()
Totalsl3o2
ggsave(plot=Totalsl3o2,filename='Totalsl3into2.png',height = 18, width =16, units = 'in')

Totalsl3o2sat <-  ggplot(Totals_lte_lite3_chloro, aes(x= CellBioVol, y=o2sat))+geom_point()+
  facet_wrap('group')+
  labs(x = "Total Biovolume",
       y = "int o2 sat")+
  theme_bw()
Totalsl3o2sat
ggsave(plot=Totalsl3o2sat,filename='Totalsl3into2sat.png',height = 18, width =16, units = 'in')

Totalsl3cond <-  ggplot(Totals_lte_lite3_chloro, aes(x= CellBioVol, y=cond))+geom_point()+
  facet_wrap('group')+
  labs(x = "Total Biovolume",
       y = "int cond")+
  theme_bw()
Totalsl3cond
ggsave(plot=Totalsl3cond,filename='Totalsl3intcond.png',height = 18, width =16, units = 'in')

Totalsl3doc.y <-  ggplot(Totals_lte_lite3_chloro, aes(x= CellBioVol, y=doc.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Total Biovolume",
       y = "int doc")+
  theme_bw()
Totalsl3doc.y
ggsave(plot=Totalsl3doc.y,filename='Totalsl3intdoc.png',height = 18, width =16, units = 'in')

Totalsl3ph.y <-  ggplot(Totals_lte_lite3_chloro, aes(x= CellBioVol, y=ph.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Total Biovolume",
       y = "int ph")+
  theme_bw()
Totalsl3ph.y
ggsave(plot= Totalsl3ph.y,filename='Totalsl3intph.png',height = 18, width =16, units = 'in')

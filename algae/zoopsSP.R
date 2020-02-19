library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(readr)
#dataset <- read_csv("lter_zoopdata_all.csv", 
#                    col_types = cols(station = col_skip()))
#View(dataset)
ZooTotals <-  read_csv('TotalZoops.csv')
ZooTotals


Cope <- ZooTotals %>%
  filter(species_code == 10000)
Cope
write.csv(Cope, 'CopeZoops.csv')

Cyclop <- ZooTotals %>%
  filter(species_code == 20000)
Cyclop
write.csv(Cyclop, 'CyclopZoops.csv')

DIACYTHOM <- ZooTotals %>%
  filter(species_code == 20302)
DIACYTHOM
write.csv(DIACYTHOM, 'DIACYTHOM.csv')

CALANOID <- ZooTotals %>%
  filter(species_code == 30000)
CALANOID
write.csv(CALANOID, 'CALANOID.csv')
#not as present during ice on#

LEPTMIN <- ZooTotals %>%
  filter(species_code == 30402)
LEPTMIN
write.csv(LEPTMIN, 'LEPTMIN.csv')

SKISTOREG <- ZooTotals %>%
  filter(species_code == 30801)
SKISTOREG
write.csv(SKISTOREG, 'SKISTOREG.csv')



xy = ggplot() + 
  geom_point(data = Cope, aes(x = sample_date, y = density), color = "red") +
  geom_point(data = Cyclop, aes(x = sample_date, y = density), color = "green") +
  geom_point(data = DIACYTHOM, aes(x = sample_date, y = density), color = "purple") +
  geom_point(data = CALANOID, aes(x = sample_date, y = density), color = "orange") +
  geom_point(data = LEPTMIN, aes(x = sample_date, y = density), color = "blue") + 
  geom_point(data = LEPTMIN, aes(x = sample_date, y = density), color = "pink") + 
  geom_point(data = LEPTMIN, aes(x = sample_date, y = density), color = "black") + 
  geom_point(data = LEPTMIN, aes(x = sample_date, y = density), color = "yellow") + 
  xlab('Year') +
  ylab('Density')+
  facet_wrap('group')
  #xlim(1997,2010)
## need to legand, and conect those dots!!
print(xy)
ggsave(plot=p,filename='intimeBiovolumes.png',height = 18, width =16, units = 'in')


cope3<- geom_point(data = Cope, aes(x = sample_date, y = density))+ 
  facet_wrap('group')

cope3
  geom_line(data = NDlte_lite3_chloro, aes(x = year4frac, y = CellBioVol), color = "blue")



  lzO<- ggplot(Limnothrixatzero2, aes(x= CellBioVol, y=o2))+geom_line()+
    facet_wrap('group')+
    labs(x = "Biovolume Limnothrix",
         y = "o2")+
    theme_bw()
  lzO









 
Microcystis <- read_csv("biovolume1.csv", col_types = cols(sampledate = col_date(format = "%m/%d/%Y"))) %>%
  filter(Genus == 'Microcystis')
Microcystisatzero<- Microcystis %>%
  right_join(zeroschem02, by= c('sampledate'))
write.csv(Microcystisatzero, "Microcystisatzero.csv")

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


dbl3light <- ggplot(Dinobryon_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=surflite))+geom_point(size=2)+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "surf light")+
  theme_bw()
dbl3light
ggsave(plot=dbl3light,filename='DBsurflight.png',height = 18, width =16, units = 'cm')
dbl3lightyear<- dbl3light+ xlab('Biovolume Dinobryon um3') + ylab('Surface Light (units)')+ ylim(0,1) + geom_point(aes(color = factor(year4.x)))
dbl3lightyear
ggsave(plot=dbl3lightyear,filename='DBsurflightyear.png',height = 18, width =16, units = 'cm')



p1 = ggplot(mtcars) + geom_point(aes(x = mpg, y = hp)) +
  ylab('horse power')
p2 = ggplot(mtcars) + geom_line(aes(x = mpg, y = qsec))
patchwork <- p1 + p2
patchwork + plot_annotation(
  tag_levels = 'a',
  title = 'Figure. a) car miles per gallon (mpg) vs horse power, b) car miles per gallon (mpg) vs qsec'
)





dbl3snow2 <- ggplot(Dinobryon_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=avsnow))+geom_point(size=4)+
  labs(x = "Biovolume Dinobryon",
       y = "av snow")+
  theme_bw()
dbl3snow2
ggsave(plot=dbl3snow2,filename='DBavsnow2.png',height = 18, width =16, units = 'in')

dbl3snowyear<- dbl3snow2+ geom_point(aes(size=3, color = factor(year4.x)))
dbl3snowyear+ xlim(0, 15000) +ylim(0,20)
ggsave(plot=dbl3snowyear,filename='DBsnowyear2.png',height = 18, width =16, units = 'in')



dbl3tice <- ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=totice))+geom_point()+
  labs(x = "Biovolume Dinobryon",
       y = "total ice cm")+
  theme_bw()+
dbl3tice
ggsave(plot=dbl3tice,filename='DBtotalice.png',height = 18, width =16, units = 'in')

dbl3tice2 <- ggplot(Dinobryon_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=totice))+geom_point(size=4)+
  labs(x = "Biovolume Dinobryon",
       y = "total ice cm")+
  theme_bw()+
  xlim(0,15000)
dbl3tice2
ggsave(plot=dbl3tice2,filename='DBtotalice2.png',height = 18, width =16, units = 'in')

dbl3tice2year<- dbl3tice2+ geom_point(aes(color = factor(year4.x)))
dbl3tice2year
ggsave(plot=dbl3tice2year,filename='dbl3tice2year2.png',height = 18, width =16, units = 'cm')

dbl3whiteice <- ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=whiteice))+geom_point()+
  labs(x = "Biovolume Dinobryon",
       y = "white ice")+
  theme_bw()
dbl3whiteice
ggsave(plot=dbl3whiteice,filename='DBwhiteice.png',height = 18, width =16, units = 'in')

dbl3whiteice2 <- ggplot(Dinobryon_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=whiteice))+geom_point(size=4)+
  labs(x = "Biovolume Dinobryon",
       y = "white ice")+
  theme_bw()+
  xlim(0,15000)
dbl3whiteice2
ggsave(plot=dbl3whiteice2,filename='DBwhiteice2.png',height = 18, width =16, units = 'cm')

dbl3whiteice2year<- dbl3whiteice2+ geom_point(aes(color = factor(year4.x)))
dbl3whiteice2year
ggsave(plot=dbl3whiteice2year,filename='dbl3whiteice2year2.png',height = 18, width =16, units = 'cm')


dbl3blueice <- ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=blueice))+geom_point()+
  labs(x = "Biovolume Dinobryon",
       y = "blue ice")+
  theme_bw()+
dbl3blueice
ggsave(plot=dbl3blueice,filename='DBlueice.png',height = 18, width =16, units = 'in')


dbl3blueice2 <- ggplot(Dinobryon_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=blueice))+geom_point(size=4)+
  labs(x = "Biovolume Dinobryon",
       y = "blue ice")+
  theme_bw()+
  xlim(0,10000)
dbl3blueice2
ggsave(plot=dbl3blueice2,filename='DBlueice2.png',height = 18, width =16, units = 'in')


dbl3blueice2year<- dbl3blueice2+ geom_point(aes(color = factor(year4.x)))
dbl3blueice2year
ggsave(plot=dbl3blueice2year,filename='dbl3blueice2year2.png',height = 18, width =16, units = 'cm')


dbl3surfchloro <-  ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=surfchlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "surface chloro")+
  theme_bw()
dbl3surfchloro
ggsave(plot=dbl3surfchloro,filename='DBsurfchloro.png',height = 18, width =16, units = 'in')

dbl3surfchloro2 <-  ggplot(Dinobryon_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=surfchlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "surface chloro")+
  theme_bw()+
  ylim(0,15)
dbl3surfchloro2
ggsave(plot=dbl3surfchloro2,filename='DBsurfchloro2.png',height = 18, width =16, units = 'in')

dbl3surfchloro2year<- dbl3surfchloro2+ geom_point(aes(color = factor(year4.x)))
dbl3surfchloro2year
ggsave(plot=dbl3surfchloro2year,filename='dbl3surfchloro2year2.png',height = 18, width =16, units = 'cm')


dbl3wtemp <-  ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=wtemp))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int wtemp")+
  theme_bw()
dbl3wtemp
ggsave(plot=dbl3wtemp,filename='DBintwtemp.png',height = 18, width =16, units = 'in')

dbl3wtemp2 <-  ggplot(Dinobryon_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=wtemp))+geom_point(size=4)+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int wtemp")+
  theme_bw()
dbl3wtemp2
ggsave(plot=dbl3wtemp2,filename='DBintwtemp2.png',height = 18, width =16, units = 'in')

dbl3wtemp2year<- dbl3wtemp2+ geom_point(aes(color = factor(year4.x)))
dbl3wtemp2year
ggsave(plot=dbl3wtemp2year,filename='dbl3wtemp2year2.png',height = 18, width =16, units = 'cm')



dbl3chloro <-  ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=chlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int chloro")+
  theme_bw()
dbl3chloro
ggsave(plot=dbl3chloro,filename='DBintchloro.png',height = 18, width =16, units = 'in')

dbl3chloro2 <-  ggplot(Dinobryon_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=chlor))+geom_point(size=4)+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int chloro")+
  theme_bw()
dbl3chloro2
ggsave(plot=dbl3chloro2,filename='DBintchloro2.png',height = 18, width =16, units = 'in')

dbl3chloro2year<- dbl3chloro2+ geom_point(aes(color = factor(year4.x)))
dbl3chloro2year
ggsave(plot=dbl3chloro2year,filename='dbl3chloro2year2.png',height = 18, width =16, units = 'cm')


dbl3o2 <-  ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=o2))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int o2")+
  theme_bw()
dbl3o2
ggsave(plot=dbl3o2,filename='DBinto2.png',height = 18, width =16, units = 'in')

dbl3o22 <-  ggplot(Dinobryon_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=o2))+geom_point(size=4)+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int o2")+
  theme_bw()
dbl3o22
ggsave(plot=dbl3o22,filename='DBinto22.png',height = 18, width =16, units = 'in')

dbl3o22year<- dbl3o22+ geom_point(aes(color = factor(year4.x)))
dbl3o22year
ggsave(plot=dbl3o22year,filename='dbl3o22year2.png',height = 18, width =16, units = 'cm')



dbl3o2sat <-  ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=o2sat))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int o2 sat")+
  theme_bw()
dbl3o2sat
ggsave(plot=dbl3o2sat,filename='DBinto2sat.png',height = 18, width =16, units = 'in')

dbl3o2sat2 <-  ggplot(Dinobryon_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=o2sat))+geom_point(size=4)+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int o2 sat")+
  theme_bw()
dbl3o2sat2
ggsave(plot=dbl3o2sat2,filename='DBinto2sat2.png',height = 18, width =16, units = 'in')

dbl3o2sat2year<- dbl3o2sat2+ geom_point(aes(color = factor(year4.x)))
dbl3o2sat2year
ggsave(plot=dbl3o22year,filename='dbl3o2sat2year2.png',height = 18, width =16, units = 'cm')



dbl3cond <-  ggplot(Dinobryon_lte_lite3_chloro, aes(x= CellBioVol, y=cond))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int cond")+
  theme_bw()
dbl3cond
ggsave(plot=dbl3cond,filename='DBintcond.png',height = 18, width =16, units = 'in')

dbl3cond2 <-  ggplot(Dinobryon_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=cond))+geom_point(size=4)+
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "int cond")+
  theme_bw()
dbl3cond2
ggsave(plot=dbl3cond2,filename='DBintcond2.png',height = 18, width =16, units = 'in')

dbl3cond2year<- dbl3cond2+ geom_point(aes(color = factor(year4.x)))
dbl3cond2year
ggsave(plot=dbl3cond2year,filename='dbl3cond2year2.png',height = 18, width =16, units = 'cm')
###############################

dbl3cond2b <-  ggplot(Dinobryon_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=cond, shape= factor(group)))+
  geom_point(size=4)+
  labs(x = "Biovolume Dinobryon",
       y = "Integrated Conductivity")+
  theme_bw()
dbl3cond2b
ggsave(plot=dbl3cond2b,filename='DBintcond2b.png',height = 18, width =16, units = 'in')

dbl3cond2yearb<- dbl3cond2+ geom_point(aes(color = factor(year4.x)))+ theme_bw()+  
  labs(x = "Biovolume Dinobryon",
      y = "Integrated Conductivity")
dbl3cond2yearb
ggsave(plot=dbl3cond2yearb,filename='dbl3cond2year2b.png',height = 18, width =16, units = 'cm')


##############################
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


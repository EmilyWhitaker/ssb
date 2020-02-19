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


SMCOl3light <- ggplot(SMCO_lte_lite3_chloro, aes(x= CellBioVol, y=surflite))+geom_point(size=4)+
  facet_wrap('group')+
  labs(x = "Biovolume SMCO",
       y = "surf light")+
  theme_bw()
SMCOl3light
ggsave(plot=SMCOl3light,filename='SMCOsurflight.png',height = 18, width =16, units = 'in')
SMCOl3light+ geom_point(aes(color = factor(surfchlor)))

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

#### tool to graph all the graphs next to eahother~~~~ and then want like types of graphs across 



#### graphed by 
library("cowplot")
SMOC_all <- plot_grid( SMCOl3snow, SMCOl3tice, SMCOl3whiteice, SMCOl3blueice, SMCOl3ph.y, SMCOl3doc.y, SMCOl3light,SMCOl3surfchloro,
          SMCOl3chloro, SMCOl3wtemp, SMCOl3o2, SMCOl3o2sat,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
          ncol = 4, nrow = 3)
ggsave(plot=SMOC_all,filename='SMOC_all.png',height = 40, width =49, units = 'in')





#### all genra by like type  ####
Snow_All <-plot_grid(lll3snow, mll3snow, pll3snow, ndl3snow, adl3snow, dbl3snow, CfKTl3snow, SMCOl3snow,
                      labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                      ncol = 3, nrow = 3)
Snow_All
ggsave(plot=Snow_All,filename='SSnow_All.png',height = 40, width =49, units = 'in')
  
Light_All <-plot_grid(lll3light, mll3light, pll3light, ndl3light, adl3light, dbl3light, CfKTl3light, SMCOl3light,
                     labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                     ncol = 3, nrow = 3)
Light_All
ggsave(plot=Light_All,filename='Light_All.png',height = 40, width =49, units = 'in')

Total_Ice_All <-plot_grid(lll3tice, mll3tice, pll3tice, ndl3tice, adl3tice, dbl3tice, CfKTl3tice, SMCOl3tice,
                      labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                      ncol = 3, nrow = 3)
Total_Ice_All
ggsave(plot=Total_Ice_All,filename='Total_Ice_All.png',height = 40, width =49, units = 'in')


Total_WhiteIce_All <-plot_grid(lll3whiteice, mll3whiteice, pll3whiteice, ndl3whiteice, adl3whiteice, dbl3whiteice, CfKTl3whiteice, SMCOl3whiteice,
                          labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                          ncol = 3, nrow = 3)
Total_WhiteIce_All
ggsave(plot=Total_WhiteIce_All,filename='Total_WhiteIce_All.png',height = 40, width =49, units = 'in')

Total_BlueIce_All <-plot_grid(lll3blueice, mll3blueice, pll3blueice, ndl3blueice, adl3blueice, dbl3blueice, CfKTl3blueice, SMCOl3blueice,
                               labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                               ncol = 3, nrow = 3)
Total_BlueIce_All
ggsave(plot=Total_BlueIce_All,filename='Total_BlueIce_All.png',height = 40, width =49, units = 'in')  


Total_SurfChloro_All <-plot_grid(lll3surfchloro, mll3surfchloro, pll3surfchloro, ndl3surfchloro, adl3surfchloro, dbl3surfchloro, CfKTl3surfchloro, SMCOl3surfchloro,
                              labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                              ncol = 3, nrow = 3)
Total_SurfChloro_All
ggsave(plot=Total_SurfChloro_All,filename='Total_SurfChloro_All.png',height = 40, width =49, units = 'in')  

Total_IntChloro_All <-plot_grid(lll3chloro, mll3chloro, pll3chloro,ndl3chloro, adl3chloro, dbl3chloro, CfKTl3chloro, SMCOl3chloro,
                                 labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                                 ncol = 3, nrow = 3)
Total_IntChloro_All
ggsave(plot=Total_IntChloro_All,filename='Total_IntChloro_All.png',height = 40, width =49, units = 'in')  


Total_WaterTemp_All <-plot_grid(lll3wtemp, mll3wtemp, pll3wtemp,ndl3wtemp, adl3wtemp, dbl3wtemp, CfKTl3wtemp, SMCOl3wtemp,
                                labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                                ncol = 3, nrow = 3)
Total_WaterTemp_All
ggsave(plot=Total_WaterTemp_All,filename='Total_WaterTemp_All.png',height = 40, width =49, units = 'in')  


Total_o2sat_All <-plot_grid(lll3o2sat, mll3o2sat, pll3o2sat,ndl3o2sat, adl3o2sat, dbl3o2sat, CfKTl3o2sat, SMCOl3o2sat,
                                labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                                ncol = 3, nrow = 3)
Total_o2sat_All
ggsave(plot=Total_o2sat_All,filename='Total_o2sat_All.png',height = 40, width =49, units = 'in')  


Total_o2_All <-plot_grid(lll3o2, mll3o2, pll3o2,ndl3o2, adl3o2, dbl3o2, CfKTl3o2, SMCOl3o2,
                         labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                         ncol = 3, nrow = 3)
Total_o2_All
ggsave(plot=Total_o2_All,filename='Total_o2_All.png',height = 40, width =49, units = 'in')  

Total_cond_All <-plot_grid(lll3cond, mll3cond, pll3cond,ndl3cond, adl3cond, dbl3cond, CfKTl3cond, SMCOl3cond,
                         labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                         ncol = 3, nrow = 3)
Total_cond_All
ggsave(plot=Total_cond_All,filename='Total_cond_All.png',height = 40, width =49, units = 'in')  


Total_doc.y_All <-plot_grid(lll3doc.y, mll3doc.y, pll3doc.y,ndl3doc.y, adl3doc.y, dbl3doc.y, CfKTl3doc.y, SMCOl3doc.y,
                           labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                           ncol = 3, nrow = 3)
Total_doc.y_All
ggsave(plot=Total_doc.y_All,filename='Total_doc_All.png',height = 40, width =49, units = 'in')  

Total_ph.y_All <-plot_grid(lll3ph.y, mll3ph.y, pll3ph.y,ndl3ph.y, adl3ph.y, dbl3ph.y, CfKTl3ph.y, SMCOl3ph.y,
                            labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                            ncol = 3, nrow = 3)
Total_ph.y_All
ggsave(plot=Total_ph.y_All,filename='Total_ph_All.png',height = 40, width =49, units = 'in')  


### awk stats###
cor.test(SMCO_lte_lite3_chloro$CellBioVol, SMCO_lte_lite3_chloro$surfchlor) #not
cor.test(SMCO_lte_lite3_chloro$CellBioVol, SMCO_lte_lite3_chloro$surflite) #pvalue, .09481


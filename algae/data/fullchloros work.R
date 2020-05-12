#Chloros work

## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2); library(ggalt) # data viz 
library(lubridate) # dealing with dates

#######

fullchloros = read.csv("data/SPFullChem.csv", stringsAsFactors = F)
fullchloros$sampledate = mdy(fullchloros$sampledate)
fullchloros %<>% subset(lakeid == "SP")
fullchloros$frlight[fullchloros$frlight=="1"] <- NA #one iceon point with no light point to calc frlight against 
fullchloros 
write.csv(fullchloros, "SPfullchloros.csv")



iceinfo = read.csv(data)


ice = read.csv('data/iceduration.csv', stringsAsFactors = F)
ice %<>% rename(ice.on = datefirstice,
                ice.off = datelastice)

ice$ice.on = mdy(ice$datefirstice)
ice$ice.off = mdy(ice$datelastice)

ice %<>% subset(year>1996 & year <2010)
ice %<>% subset(lakeid == "SP")

ice.labs <- c("Ice Off", "Ice On")
names(ice.labs) <- c(0, 1)






#graph chloro.int over time 

ggplot(fullchloros, aes(sampledate, chlor))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_smooth()+
  theme_classic()

ggplot(fullchloros, aes(sampledate, chlor))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  scale_color_viridis_c(option="viridis")+
  geom_smooth()+
  #scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))+
  theme_classic()+
  labs(x='Year')


#######
surfchloros = read.csv("data/surfacechloro.csv", stringsAsFactors = F)
surfchloros %<>% subset(depth == 0)
surfchloros$sampledate = ymd(surfchloros$sampledate)
write.csv(surfchloros, "SPsurfchloros.csv")
surfchloros

ggplot(surfchloros, aes(sampledate, chlor))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  scale_color_viridis_c(option="viridis")+
  geom_smooth()+
  #scale_color_brewer(palette = 'Paired')+
  theme_classic()+
  labs(x='Year')

#######
allchlor= read.csv("surfandintSPfullchloros.csv")
allchlor$sampledate = mdy(allchlor$sampledate)


ggplot((subset(allchlor, ice.pres %in% 1)), aes(sampledate, chlor.surf))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  #geom_point()+
  scale_color_viridis_c(option="viridis")+
  geom_smooth(method=lm,se=T)+
  #scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))+
  theme_classic()+
  geom_encircle(aes(x=sampledate, y=chlor.surf), 
                data=winter, 
                color="red", 
                size=2, 
                expand=0.08) + 
  labs(x='Year')



totaladjchloro=
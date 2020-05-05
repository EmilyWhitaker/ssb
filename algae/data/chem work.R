#okay damn

## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz 
library(lubridate) # dealing with dates

#####============================

totals = read.csv("data/clean_abiotic_genus_03262020.csv", stringsAsFactors = F)
totals$sampledate = mdy(totals$sampledate)


#define seasons
ice = read.csv('data/iceduration.csv', stringsAsFactors = F)
ice %<>% rename(ice.on = datefirstice,
                ice.off = datelastice)

ice$ice.on = mdy(ice$datefirstice)
ice$ice.off = mdy(ice$datelastice)

ice %<>% subset(year>1996 & year <2010)
ice %<>% subset(lakeid == "SP")

ice.labs <- c("Ice Off", "Ice On")
names(ice.labs) <- c(0, 1)




	
"tpm","cl",	"so4",	"ca",	"mg",	"na",	"k",	"fe", "mn",	"chlor.surf",	"avsnow",	"totice",
"whiteice", "blueice","light","iceduration"), names_to="variable", values_to = "value"))


#value by year 
ggplot(totals, aes(sampledate, iceduration))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_smooth()+
  theme_classic()

#in the ~seasons~

ggplot(totals, aes(sampledate, iceduration))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_smooth(method=lm)+
  #scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))+
  theme_classic()+
  labs(x='Year')

ggplot(totals, aes(sampledate, cond))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point(totals, aes(sampledate, cond))+
  geom_smooth(totals, aes(sampledate, cond), method='lm', se=F)+
  #scale_color_brewer(palette = 'Paired')+
  #facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))+
  theme_classic()+
  labs(x='Year')

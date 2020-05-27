#figuring out FRLight

## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin)

#####=======

lightfunction = read.csv('data/chemphys.csv', stringsAsFactors = F)
lightfunction$sampledate = ymd(lightfunction$sampledate)
lightfunction %<>%  subset(lightfunction$depth==0)
lightfunction$frlight[lightfunction$frlight>="1"] <- NA

max(lightfunction$frlight)

max(lightfunction$frlight, na.rm=TRUE)

ggplot(lightfunction, aes(sampledate, frlight))+
  geom_point()+
  geom_smooth()

#break up frlight with winter only times?


snowice = read.csv('data/snowicedepth.csv', stringsAsFactors = F)
snowice%<>%subset(lakeid == "SP") %>%
  select(lakeid, year4, sampledate, avsnow, totice, whiteice, blueice)
snowice$sampledate = ymd(snowice$sampledate)

frsnow= right_join(lightfunction,snowice, by='sampledate', 'lakeid')

ggplot(frsnow, aes(frlight, avsnow))+
  geom_point()+
  geom_smooth()

ggplot(frsnow, aes(frlight, totice))+
  geom_point()+
  geom_smooth()


ggplot(frsnow, aes(frlight, whiteice))+
  geom_point()+
  geom_smooth()

ggplot(frsnow, aes(frlight, blueice))+
  geom_point()

ggplot(frsnow, aes(blueice, frlight))+
  geom_point()


ggplot(winter.data.total.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')
#Trout Bog snow and ice over time 

## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin)

############

data = read.csv('data/Trout data/snowiceTB.csv', stringsAsFactors = F)
data$sampledate = ymd(data$sampledate)
data %<>% rename('white ice' = whiteice)
data %<>% rename('blue ice' = blueice)
data.long = pivot_longer(data, cols=c("totice","white ice","blue ice"), names_to="variable", values_to = "value")

data.long2 = pivot_longer(data, cols=c("white ice","blue ice"), names_to="variable", values_to = "value")



ggplot(data.long, aes(sampledate, value,color=variable))+
  ylab('cm')+
  geom_point()+
  geom_smooth(aes(group=variable))+
  geom_bar(aes(group=variable))+
  geom_smooth(aes(group=variable))

ggplot(data.long2, aes(fill=variable, y=value, x=sampledate))+
  xlab("Sample Date")+
  ylab("Ice Thickness (cm)")+
  labs(fill='Ice Composition')+
  theme_update(text = element_text(size=25))+
  theme_classic()+
#  theme(axis.text.x=element_text(size=rel(1.5)))+
  geom_bar(position = "stack", stat= "identity", width = 80)




ggplot(data, aes(sampledate, value,color=variable))+
  ylab('cm')+
  geom_point()+
  geom_smooth(aes(group=variable))+
  geom_bar(aes(group=variable))+
  geom_smooth(aes(group=variable))

ggplot(data, aes(y=avsnow, x=sampledate))+
  xlab("Sample Date")+
  ylab("Average Snow (cm)")+
  theme_update(text = element_text(size=600))+
  theme_classic()+
  geom_bar(stat= "identity", width= 100, fill= "grey")

ggplot(data, aes(sampledate, avsnow))+
  ylab('average snow fall (cm)')+
  xlab('sample date')+
  geom_point()+
  geom_smooth()

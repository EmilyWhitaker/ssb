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

data.long = pivot_longer(data, cols=c("totice","whiteice","blueice"), names_to="variable", values_to = "value")

ggplot(data.long, aes(sampledate, value,color=variable))+
  ylab('cm')+
  geom_point()+
  geom_smooth(aes(group=variable))+
  geom_line(aes(group=variable))+
  geom_smooth(aes(group=variable))


ggplot(data, aes(sampledate, avsnow))+
  ylab('average snow fall (cm)')+
  xlab('sample date')+
  geom_point()+
  geom_smooth()

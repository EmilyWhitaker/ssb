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

lightext = read.csv("data/light_ext_v1-2.csv", stringsAsFactors = F)
lightext$sampledate = mdy(lightext$sampledate)


data = full_join(totals, lightext, by=c("sampledate"))


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


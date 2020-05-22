## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin)


######====
#Hilary's example
chlorophyll = data.frame(chloroDate = c(as.Date('2000-01-30'), as.Date('2003-01-30'),
                                        as.Date('2004-01-30'), as.Date('2006-01-30')),
                         num = c(23,12,16,18))
# Create columns of date range. In this case, date + 1, and date - 1
phytos = data.frame(phytoDate = c(as.Date('2000-01-30'), as.Date('2003-01-29'), as.Date('2006-01-30')),
                    num2 = c(20:22)) %>%
  mutate(datePlus1 = phytoDate + 1) %>% mutate(dateMinus1 = phytoDate - 1)
# Use a fuzzy join to compare the two columns to the chlorophyll date
fuzzy_left_join(chlorophyll, phytos, by = c("chloroDate" = "datePlus1", "chloroDate" = "dateMinus1"),
                match_fun = list(`<=`, `>=`))

#######

chl = read.csv('data/chloro_all.csv',stringsAsFactors = F)
chl %<>% subset(lakeid == "SP" & depth == 0) %>%
  select(year4, daynum, sampledate, chlor)
chl$sampledate =  ymd(chl$sampledate)

#Inegrated Chems
intchems = read.csv("data/SPFullChem.csv", stringsAsFactors = F)
intchems$sampledate = mdy(intchems$sampledate)
intchems %<>% subset(lakeid == "SP")
intchems$frlight[intchems$frlight=="1"] <- NA #one iceon point with no light point to calc frlight against
intchems

intchems2 = intchems %>% select(intchemsDate = sampledate, wtemp:mn)

chl2 = chl %>% select(chlDate = sampledate,chlor) %>%
  mutate(datePlus1 = chlDate + 1) %>% mutate(dateMinus1 = chlDate - 1)

join <- fuzzy_left_join(intchems2, chl2, by = c("intchemsDate" = "datePlus1", "intchemsDate" = "dateMinus1"),
                match_fun = list(`<=`, `>=`))

join %<>% rename(sampledate = intchemsDate)

ice = read.csv('data/snowicedepth.csv',stringsAsFactors = F)
ice %<>% subset(lakeid == "SP") %>%
  select(year4, daynum, sampledate, avsnow, totice, whiteice, blueice)
ice$sampledate = ymd(ice$sampledate)

join_ice <- left_join(join, ice, by= c('sampledate'))

totalbv = read.csv("data/TotalBVs.csv", stringsAsFactors = F)
totalbv= read.csv("../TotalBVs.csv")
totalbv$sampledate = ymd(totalbv$sampledate)
totalbv %<>% select(sampledate, Genus, CellBioVol) %>%
  mutate(bv.datePlus1 = sampledate + 1) %>% mutate(bv.dateMinus1 = sampledate - 1)

join_surfchlor <-fuzzy_left_join(join_ice, totalbv, by = c("sampledate" = "bv.datePlus1", "sampledate" = "bv.dateMinus1"),
                       match_fun = list(`<=`, `>=`))

join_surfchlor %<>% rename(sampledate= sampledate.y)

write.csv(join_surfchlor, 'data/datasetcleannogenera.csv', row.names = F)


class(join_surfchlor$sampledate)

Genera = read.csv("data/CleanBiovolumes05202020", stringsAsFactors = F)
Genera$sampledate = ymd(Genera$sampledate)

fulldatasetclean05202020 = full_join(join_surfchlor, Genera, by=c('sampledate'))
write.csv(fulldatasetclean05202020, 'data/fulldatasetclean05202020.csv', row.names = F)

######
#Cleaning col names

fulldatasetclean05202020 %<>% rename(chlor.int = intchlor)
fulldatasetclean05202020 %<>% rename(chlor.surf = chlor.y)

fulldatasetclean05202020_2= fulldatasetclean05202020 %>% select(sampledate.x:chlor.surf, year4,
                                                                avsnow:blueice,Genus.y:CellBioVol.y)
  
fulldatasetclean05202020_2%<>% rename(Genus = Genus.y)
fulldatasetclean05202020_2%<>% rename(CellBioVol = CellBioVol.y)
fulldatasetclean05202020_2%<>% rename(sampledate = sampledate.x)

fulldatasetclean05202020_2$avsnow[is.na(fulldatasetclean05202020_2$avsnow)]=0
fulldatasetclean05202020_2$totice[is.na(fulldatasetclean05202020_2$totice)]=0
fulldatasetclean05202020_2$blueice[is.na(fulldatasetclean05202020_2$blueice)]=0
fulldatasetclean05202020_2$whiteice[is.na(fulldatasetclean05202020_2$whiteice)]=0
fulldatasetclean05202020_2$year = year(fulldatasetclean05202020_2$sampledate)

# load dataset of iceon and off dates
ice = read.csv('data/iceduration.csv', stringsAsFactors = F)
ice %<>% rename(ice.on = datefirstice,
                ice.off = datelastice)

ice$ice.on = mdy(ice$ice.on)
ice$ice.off = mdy(ice$ice.off)
ice %<>% subset(lakeid == "SP") %>%
  select(year,ice.off, ice.on, iceduration)

fulldatasetclean05202020_3 = full_join(fulldatasetclean05202020_2, ice, by=c('year'))



write.csv(fulldatasetclean05202020_3, 'data/FINALfulldatasetclean05202020.csv', row.names = F)




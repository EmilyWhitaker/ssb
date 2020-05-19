## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz 
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin)

######====

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



ice = read.csv('data/snowicedepth.csv',stringsAsFactors = F)
ice %<>% subset(lakeid == "SP") %>%
  select(year4, daynum, sampledate, avsnow, totice, whiteice, blueice) %>%
  mutate(perwhiteice=(whiteice/totice)*100, 
         perblueice=(blueice/totice)*100)
ice$sampledate = ymd(ice$sampledate)


chl = read.csv('data/chloro_all.csv',stringsAsFactors = F)
chl %<>% subset(lakeid == "SP" & depth == 0) %>%
  select(year4, daynum, sampledate, chlor)
chl$sampledate =  ymd(chl$sampledate)
ice2= data.frame(iceDate = c(ice$sampledate))

#phytos 
#Inegrated Chems 
intchems = read.csv("data/SPFullChem.csv", stringsAsFactors = F)
intchems$sampledate = mdy(intchems$sampledate)
intchems %<>% subset(lakeid == "SP")
intchems$frlight[intchems$frlight=="1"] <- NA #one iceon point with no light point to calc frlight against 
intchems %>%
  rename(chlor.int= chlor)


intchems2= data.frame(intchemsDate = c(intchems$sampledate),
                     intchems$wtemp, intchems$o2, intchems$o2sat, intchems$cond, intchems$frlight,
                     intchems$chlor, intchems$phaeo, intchems$ph, intchems$phair, intchems$alk,
                     intchems$dic, intchems$tic, intchems$doc, intchems$toc, intchems$no3no2,
                     intchems$no2, intchems$nh4, intchems$totnf, intchems$totnuf, intchems$totpf,
                     intchems$totpuf, intchems$drsif, intchems$brsif, intchems$brsiuf, intchems$tpm,
                     intchems$cl, intchems$so4, intchems$ca, intchems$mg, intchems$na, intchems$k,
                     intchems$fe, intchems$mn)
chl2=data.frame(chlDate= c(chl$sampledate),chl$chlor)%>% 
  mutate(datePlus1 = chlDate + 1) %>% mutate(dateMinus1 = chlDate - 1)
join <-fuzzy_left_join(intchems2, chl2, by = c("intchemsDate" = "datePlus1", "intchemsDate" = "dateMinus1"), 
                match_fun = list(`<=`, `>=`))

join %<>% rename(sampledate = intchemsDate)


#still need light, ice, and bio vol info 

join_ice <- left_join(join, ice, by= c('sampledate'))

#need light and bv




#ice2= data.frame(iceDate = c(ice$sampledate),ice$avsnow, ice$totice, ice$whiteice, ice$blueice)

#join2<- fuzzy_left_join(join, ice2, by = c("intchemsDate" = "datePlus1", "intchemsDate" = "dateMinus1"), 
#                        match_fun = list(`<=`, `>=`))




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



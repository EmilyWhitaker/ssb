## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz 
library(lubridate) # dealing with dates

#===========
# NO NEED TO RERUN - SKIP AHEAD 
# deal with abiotic data

chl = read.csv('data/chloro_all.csv',stringsAsFactors = F)
chl %<>% subset(lakeid == "SP" & depth == 0) %>%
  select(year4, daynum, sampledate, chlor)
chl$sampledate =  ymd(chl$sampledate)
 
ice = read.csv('data/snowicedepth.csv',stringsAsFactors = F)
ice %<>% subset(lakeid == "SP") %>%
  select(year4, daynum, sampledate, avsnow, totice, whiteice, blueice) %>%
  mutate(perwhiteice=(whiteice/totice)*100, 
         perblueice=(blueice/totice)*100)
ice$sampledate = ymd(ice$sampledate)

abiotic = full_join(chl, ice, by=c('year4',"daynum",'sampledate'))
abiotic %<>% rename(year = year4)

new = read.csv('data/newfile.csv', stringsAsFactors = F)
new %<>% select(sampledate, light)
new$sampledate = mdy(new$sampledate)
new$year = year(new$sampledate)
new$daynum = yday(new$sampledate)

abiotic = full_join(abiotic, new, by=c("year","daynum","sampledate"))

# deal with algae dataset
algae = read.csv('data/biovolume1.csv',stringsAsFactors = F)

algae %<>% select(sampledate, Genus, CellBioVol)

algae$sampledate = mdy(algae$sampledate)
algae$daynum = yday(algae$sampledate)
algae$year = year(algae$sampledate)

data = full_join(abiotic, algae, by=c("year","daynum","sampledate"))

data %<>% subset(year > 1996)

write.csv(data, 'data/clean_algae_abiotic_02252020.csv',row.names = F)


#======================
data = read.csv('data/clean_algae_abiotic_02252020.csv',stringsAsFactors = F)



# pull out total biovolumes
totals = subset(data, Genus == "TotalBiovolume")
totals$chlor = abs(totals$chlor)

# pull out genus-specific biovolumes
genus = subset(data, Genus != "TotalBiovolume")

# paired-plot of predictors and total biovolumes for co-linearity
pairs(totals[,c(4:11,13)])

# assess distrubition of variables
hist(totals$chlor)
hist(log(totals$chlor)) # log chlor

hist(totals$avsnow) # no transform

hist(totals$totice)
hist(log(totals$totice)) # bit better log

hist(totals$whiteice) # no transform

hist(totals$blueice) 
hist(log(totals$blueice)) # bit better log, maybe unneccessary?

hist(totals$light)
hist(log(totals$light)) # bit better log

hist(totals$CellBioVol)
hist(log(totals$CellBioVol)) # log bv

#=========
totals %<>% select(-Genus)
totals$log.chlor = log(totals$chlor)
totals$log.bv = log(totals$CellBioVol)
totals.long = pivot_longer(totals, cols=c("chlor","log.chlor","avsnow","totice","whiteice","blueice","perwhiteice","perblueice",
                                  "light","CellBioVol","log.bv"), names_to="variable", values_to = "value")




ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')

ggplot(totals.long, aes(year, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')

totals.long$month = month(totals.long$sampledate)

winter = subset(totals.long, month < 4)

#only pull out winter sampling
ggplot(winter, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')

ggplot(winter, aes(year, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')

ggplot(winter, aes(year, value, color=variable))+
  geom_point()+
  geom_smooth(method='lm', aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')


#==============
# check temporal bv trend 
totals$month = month(totals$sampledate)
winter.totals = subset(totals, month <4)

winter.totals$ctr.year = winter.totals$year-mean(winter.totals$year)

# temporal only, no random effects
bv.1 = lm(log.bv ~ ctr.year, data=winter.totals)
AIC(bv.1) 
summary(bv.1) # not signif

# random intercept - temporal only
bv.2 = lme(log.bv ~ ctr.year, random = ~1|ctr.year, method ='REML', data=winter.totals)
summary(bv.2) # not sig


model = subset(winter.totals,!is.na(chlor))
model.2 = subset(model, !is.na(light))


# 2. begin incorporating random effects 's(year,bs="re")' 
bv.a = lme(log.bv ~ log.chlor + avsnow + totice + blueice + light, random = ~1|ctr.year, data=model.2)
summary(bv.a) # not significant

# get rid of chloro - least sig, also proxy(ish) for bv
bv.b = lme(log.bv ~ avsnow + totice + blueice + light, random = ~1|ctr.year, data=model.2)
summary(bv.b) # not significant

# get rid of light - least sig
bv.c = lme(log.bv ~ avsnow + totice + blueice, random = ~1|ctr.year, data=model)
summary(bv.c) # blue ice not significant

# get rid of blue ice - least sig
bv.d = lme(log.bv ~ avsnow + totice, random = ~1|ctr.year, data=model)
summary(bv.d) # blue ice not significant

# get rid of total ice - least sig
bv.e = lme(log.bv ~ avsnow + blueice, random = ~1|ctr.year, data=model)
summary(bv.e) # blue ice not significant

# get rid of total ice - least sig
bv.f = lme(log.bv ~ avsnow, random = ~1|ctr.year, data=model)
summary(bv.f) # significant

#========
# genus trends over time

unique(genus$Genus) # shows all names there

genus[genus=="unID cyanobacteria (colony)"] <- NA

start = c("Limnothrix", "Microcystis", "Peanut", "Naked Dinoflagellate", "Dinobryon", "Armored Dinoflagellate")

ggplot(subset(genus,Genus %in% start), aes(year, log(CellBioVol), color=Genus))+
  geom_point(aes(group=Genus))+
  geom_smooth(aes(group=Genus),se=F)











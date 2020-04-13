## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz 
library(lubridate) # dealing with dates

#===================
# load sparkling abiotic info
abiotic = read.csv('data/SPFullChem.csv', stringsAsFactors = F)

# change date format and limit to 1997-2009
abiotic$sampledate = mdy(abiotic$sampledate)
abiotic$year = year(abiotic$sampledate)
abiotic %<>% subset(year > 1996 & year < 2010)

# get rid of a few unneeded columns
abiotic = abiotic[,-c(1,3,37:42)]


#======
# phytoplankton data
# read in total biovolume data
data = read.csv('data/clean_algae_abiotic_03032020.csv',stringsAsFactors = F)

# pull out total biovolumes
totals = subset(data, Genus == "TotalBiovolume")
totals$chlor = abs(totals$chlor)
totals$sampledate = ymd(totals$sampledate)

totals %<>% select(-c(perwhiteice, perblueice))
# there are multiple (and different) chlorophyll values for each sample date - will want to figure out why differnet and pick 1?
# chlor.y also has multiple chlorophyll values per date - going to aggregate to average per date

# join abiotic data with totals data
ab.tbv = full_join(abiotic, totals, by="sampledate")
ab.tbv %<>% select(-c(year.y, daynum, Genus))

# subset data to only be when have phytoplankton data
ab.tbv %<>% subset(!is.na(CellBioVol)) %>%
  rename(year = year.x)

ab.tbv$year = year(ab.tbv$sampledate)

# load dataset of iceon and off dates
ice = read.csv('data/iceduration.csv', stringsAsFactors = F)
ice %<>% rename(ice.on = datefirstice,
                ice.off = datelastice)

ice$ice.on = mdy(ice$ice.on)
ice$ice.off = mdy(ice$ice.off)

ice %<>% subset(year>1996 & year <2010)
ice %<>% subset(lakeid == "SP")
ice %<>% select(year, ice.off, ice.on,iceduration)

ab.tbv = left_join(ab.tbv, ice, by="year")

ab.tbv$ice.pres = ifelse(ab.tbv$sampledate < ab.tbv$ice.off, 1, 0)

# subset periods when no ice - add zeros for ice and snow data
ab.tbv.no.ice = subset(ab.tbv, ice.pres == 0)
ab.tbv.ice = subset(ab.tbv, ice.pres == 1)

ab.tbv.no.ice$avsnow[is.na(ab.tbv.no.ice$avsnow)] = 0
ab.tbv.no.ice$totice[is.na(ab.tbv.no.ice$totice)] = 0
ab.tbv.no.ice$whiteice[is.na(ab.tbv.no.ice$whiteice)] = 0
ab.tbv.no.ice$blueice[is.na(ab.tbv.no.ice$blueice)] = 0

ab.tbv = rbind(ab.tbv.ice, ab.tbv.no.ice)

ab.tbv = unique(ab.tbv)
ab.tbv %<>% rename(chlor.int = chlor.x,
                   chlor.surf = chlor.y)

write.csv(ab.tbv, 'data/abiotic_total_biovolume.csv', row.names = F)

# look at tbv over time
hist(ab.tbv$CellBioVol)
hist(log(ab.tbv$CellBioVol))
shapiro.test(log(ab.tbv$CellBioVol))

ggplot(ab.tbv, aes(year, log(CellBioVol)))+
  geom_point()+
  geom_smooth()

hist(ab.tbv$chlor.int)
hist(sqrt(ab.tbv$chlor.int))
shapiro.test(sqrt(ab.tbv$chlor.int))

ggplot(ab.tbv, aes(sqrt(chlor.int), log(CellBioVol)))+
  geom_point()+
  geom_smooth(method='lm')

hist(ab.tbv$chlor.surf)
hist(sqrt(ab.tbv$chlor.surf))
shapiro.test(sqrt(ab.tbv$chlor.surf))

ggplot(ab.tbv, aes(sqrt(chlor.surf), log(CellBioVol)))+
  geom_point()+
  geom_smooth(method='lm')

#============
# link abiotic info to genus data
ab.tbv %<>% select(-CellBioVol)

# read-in genus-specific data
genus = read.csv('data/genus_clean_03032020.csv', stringsAsFactors = F)
#genus$CellBioVol[genus$CellBioVol==0] <- NA

genus %<>% select(sampledate, Genus, CellBioVol)
genus$sampledate = ymd(genus$sampledate)

ab.genus.bv = full_join(ab.tbv, genus, by="sampledate")

write.csv(ab.genus.bv, 'data/abiotic_genus_biovolume.csv', row.names = F)




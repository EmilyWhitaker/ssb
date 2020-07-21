#2010-2015 North sampling (TB and SP data)

# plot with light, ice type, thickness, zoops, chl-a

## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin)


#######

#merging TB dataset-- eek 
#TB_dataphyto = read.csv('data/TB_data_phytos.csv', stringsAsFactors = F)
#TB_dataphyto$sampledate = ymd(TB_dataphyto$sampledate)

TB_data_phytos <- read.csv("data/TB_data_phytos_totals_withNA.csv", na = "null")
TB_data_phytos$sampledate = mdy(TB_data_phytos$sampledate)

####### correct merge#############
uniqueTaxa = TB_data_phytos %>% select(division, genus, taxa_name) %>% 
  distinct()
uniqueDates = TB_data_phytos %>% select(sampledate) %>% 
  distinct()
combo = tidyr::expand_grid(uniqueDates, uniqueTaxa) %>% 
  left_join(TB_data_phytos, by = c("sampledate", "division", "genus", "taxa_name"))
combo2= combo %>% 
  mutate(biovolume_conc = if_else(is.na(biovolume_conc), 0, biovolume_conc))
combo3= combo2 %>% 
  mutate(relative_total_biovolume = if_else(is.na(relative_total_biovolume), 0, relative_total_biovolume))
write.csv(combo3, 'data/TBPhytos_Clean.csv', row.names = F)

############
#TB ZOOPS

TB.zoops.code= read.csv('data/TB_zoops.csv', stringsAsFactors = F)
TB.zoops.code$sampledate = mdy(TB.zoops.code$sampledate)
#need to make all of the absence data
#TB.zoops.code %<>% select(sampledate, species_code, density)

uniqueZoop = TB.zoops.code %>% select(species_code, species_name) %>% 
  distinct()
uniqueDates = TB.zoops.code %>% select(sampledate) %>% 
  distinct()
combo = tidyr::expand_grid(uniqueDates, uniqueZoop) %>% 
  left_join(TB.zoops.code, by = c("sampledate", "species_code", "species_name"))
combo2= combo %>% 
  mutate(density = if_else(is.na(density), 0, density))
write.csv(combo2, 'data/TBZoops_Clean.csv', row.names = F)

#probably should cut this data short

####SP merging and zeroing#######

SP_data_phytos = read.csv('data/SP_data_phytos_totals.csv', stringsAsFactors = F)
SP_data_phytos$sampledate = mdy(SP_data_phytos$sampledate)

uniqueTaxa = SP_data_phytos %>% select(division, genus, taxa_name) %>% 
  distinct()
uniqueDates = SP_data_phytos %>% select(sampledate) %>% 
  distinct()
combo = tidyr::expand_grid(uniqueDates, uniqueTaxa) %>% 
  left_join(SP_data_phytos, by = c("sampledate", "division", "genus", "taxa_name"))
combo2= combo %>% 
  mutate(biovolume_conc = if_else(is.na(biovolume_conc), 0, biovolume_conc))
combo3= combo2 %>% 
  mutate(relative_total_biovolume = if_else(is.na(relative_total_biovolume), 0, relative_total_biovolume))
combo4= combo3 %>% 
  mutate(biomass_conc = if_else(is.na(biomass_conc), 0, biomass_conc))
combo5= combo4 %>% 
  mutate(cells_per_ml = if_else(is.na(cells_per_ml), 0, cells_per_ml))
write.csv(combo5, 'data/SPPhytos_Clean.csv', row.names = F)

SP_Phytos_clean=combo5

#SP_Chems1 = read.csv("data/fulldatasetclean05202020.csv", stringsAsFactors = F)

chl = read.csv('data/chloro_all.csv',stringsAsFactors = F)
chl %<>% subset(lakeid == "SP" & depth == 0) %>%
  select(year4, daynum, sampledate, chlor)
chl$sampledate =  ymd(chl$sampledate)

#Inegrated Chems
intchems = read.csv("data/SPFullChem.csv", stringsAsFactors = F)
intchems$sampledate = mdy(intchems$sampledate)
intchems %<>% subset(lakeid == "SP")
intchems$frlight[intchems$frlight=="1"] <- NA #one iceon point with no light point to calc frlight against
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

write.csv(join_ice, 'data/joinedbioticandaboitic.csv', row.names = F)

SP_Phytos_clean %<>% select(sampledate, division, taxa_name, genus, biovolume_conc, cells_per_ml, relative_total_biovolume) %>%
  mutate(bv.datePlus1 = sampledate + 1) %>% mutate(bv.dateMinus1 = sampledate - 1)

join_surfchlor_SP <-fuzzy_left_join(join_ice, SP_Phytos_clean, by = c("sampledate" = "bv.datePlus1", "sampledate" = "bv.dateMinus1"),
                                    match_fun = list(`<=`, `>=`))

join_surfchlor_SP %<>% rename(sampledate= sampledate.y)

join_surfchlor_SP %<>% rename(chlor.int = chlor.x)
join_surfchlor_SP %<>% rename(chlor.surf = chlor.y)
join_surfchlor_SP$avsnow[is.na(join_surfchlor_SP$avsnow)]=0
join_surfchlor_SP$totice[is.na(join_surfchlor_SP$totice)]=0
join_surfchlor_SP$blueice[is.na(join_surfchlor_SP$blueice)]=0
join_surfchlor_SP$whiteice[is.na(join_surfchlor_SP$whiteice)]=0
#join_surfchlor_SP %<>% rename(sampledate = sampledate.x)
#join_surfchlor_SP %<>% rename(biovolume = biovolume_conc)  
write.csv(SPdataset, 'data/cleanSPdataset.csv', row.names = F)


#SPdataset.dates.clean.iceoff_nozeros= subset(SPdataset.dates.clean.iceoff, doc>=0)
#SPdataset.dates.clean.iceoff_nozeros= subset(SPdataset.dates.clean.iceoff, toc>=0)
#SPdataset.dates.clean.iceoff_nozeros= subset(SPdataset.dates.clean.iceoff, no3no2>=0)
#SPdataset.dates.clean.iceoff_nozeros= subset(SPdataset.dates.clean.iceoff, nh4>=0)
#SPdataset.dates.clean.iceoff_nozeros= subset(SPdataset.dates.clean.iceoff_nozeros, toc>=0)


# SP Zoops 
SPZoops = read.csv('data/SPZoops.csv',stringsAsFactors = F)
SPZoops$sampledate = mdy(SPZoops$sampledate)

uniqueZoops = SPZoops %>% select(species_code, species_name) %>% 
  distinct()
uniqueDates = SPZoops %>% select(sampledate) %>% 
  distinct()
combo = tidyr::expand_grid(uniqueDates, uniqueZoops) %>% 
  left_join(SPZoops, by = c("sampledate", "species_code", "species_name"))
combo2= combo %>% 
  mutate(density = if_else(is.na(density), 0, density))
CleanSPZoops=combo2
write.csv(CleanSPZoops, 'data/CleanSPZoops.csv', row.names = F)
SPZoops = read.csv('data/CleanSPZoops.csv',stringsAsFactors = F)
SPZoops$sampledate = ymd(SPZoops$sampledate)
SPZoops$year = year(SPZoops$sampledate)

CleanSPZoops.subset = subset(SPZoops, year >= 2008)

#cut this data short?

join_ice = read.csv('data/joinedbioticandaboitic.csv',stringsAsFactors = F)
join_ice$sampledate = ymd(join_ice$sampledate)
SP_Phytos_clean = read.csv('data/SPPhytos_Clean.csv',stringsAsFactors = F)
SP_Phytos_clean$sampledate = ymd(SP_Phytos_clean$sampledate)

SP_Phytos_clean %<>% select(sampledate, division, taxa_name, genus, biovolume_conc, cells_per_ml, relative_total_biovolume) %>%
  mutate(bv.datePlus1 = sampledate + 1) %>% mutate(bv.dateMinus1 = sampledate - 1)

join_surfchlor_SP <-fuzzy_left_join(join_ice, SP_Phytos_clean, by = c("sampledate" = "bv.datePlus1", "sampledate" = "bv.dateMinus1"),
                                    match_fun = list(`<=`, `>=`))

join_surfchlor_SP %<>% rename(sampledate= sampledate.y)

join_surfchlor_SP %<>% rename(chlor.int = chlor.x)
join_surfchlor_SP %<>% rename(chlor.surf = chlor.y)
join_surfchlor_SP$avsnow[is.na(join_surfchlor_SP$avsnow)]=0
join_surfchlor_SP$totice[is.na(join_surfchlor_SP$totice)]=0
join_surfchlor_SP$blueice[is.na(join_surfchlor_SP$blueice)]=0
join_surfchlor_SP$whiteice[is.na(join_surfchlor_SP$whiteice)]=0


#CleanSPZoops.subset %<>% select(sampledate, species_code, species_name, density, individuals_measured, avg_length) %>%
#  mutate(cl.datePlus1 = sampledate + 1) %>% mutate(cl.dateMinus1 = sampledate - 1)

#SPdataset.clean <-fuzzy_left_join(join_surfchlor_SP, CleanSPZoops.subset, by = c("sampledate.x" = "cl.datePlus1", "sampledate.x" = "cl.dateMinus1"),
#                                    match_fun = list(`<=`, `>=`))



CleanSPZoops = CleanSPZoops.subset %>% select(SPZoopDate = sampledate, species_code, species_name, density, individuals_measured, avg_length) %>%
  mutate(datePlus1 = SPZoopDate + 1) %>% mutate(dateMinus1 = SPZoopDate - 1)

SPdataset.clean  <- fuzzy_left_join(join_surfchlor_SP, CleanSPZoops, by = c("sampledate.x" = "datePlus1", "sampledate.x" = "dateMinus1"),
                        match_fun = list(`<=`, `>=`))





#join_surfchlor_SP %<>% rename(sampledate = sampledate.x)
#join_surfchlor_SP %<>% rename(biovolume = biovolume_conc)




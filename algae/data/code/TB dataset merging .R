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

uniqueZoop = TB.zoops.code %>% select(species_code, density) %>% 
  distinct()
uniqueDates = TB.zoops.code %>% select(sampledate) %>% 
  distinct()
combo = tidyr::expand_grid(uniqueDates, uniqueZoop) %>% 
  left_join(TB.zoops.code, by = c("sampledate", "species_code", "density"))
combo2= combo %>% 
  mutate(density = if_else(is.na(density), 0, density))
write.csv(combo2, 'data/TBZoops_Clean.csv', row.names = F)


combo3= combo2 %>% 
  mutate(relative_total_biovolume = if_else(is.na(relative_total_biovolume), 0, relative_total_biovolume))
write.csv(combo3, 'data/TBZoops_Clean.csv', row.names = F)



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

SP_Phytos_clean %<>% select(sampledate, division, taxa_name, genus, biovolume_conc, Season) %>%
  mutate(bv.datePlus1 = sampledate + 1) %>% mutate(bv.dateMinus1 = sampledate - 1)

join_surfchlor_SP <-fuzzy_left_join(join_ice, SP_Phytos_clean, by = c("sampledate" = "bv.datePlus1", "sampledate" = "bv.dateMinus1"),
                                    match_fun = list(`<=`, `>=`))

join_surfchlor_SP %<>% rename(sampledate= sampledate.y)




SPdataset.dates.clean.iceoff_nozeros= subset(SPdataset.dates.clean.iceoff, doc>=0)
SPdataset.dates.clean.iceoff_nozeros= subset(SPdataset.dates.clean.iceoff, toc>=0)
SPdataset.dates.clean.iceoff_nozeros= subset(SPdataset.dates.clean.iceoff, no3no2>=0)
SPdataset.dates.clean.iceoff_nozeros= subset(SPdataset.dates.clean.iceoff, nh4>=0)
SPdataset.dates.clean.iceoff_nozeros= subset(SPdataset.dates.clean.iceoff_nozeros, toc>=0)
















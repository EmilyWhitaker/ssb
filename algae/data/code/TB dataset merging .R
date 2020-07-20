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













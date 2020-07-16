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
TB_dataphyto = read.csv('data/TB_data_phytos.csv', stringsAsFactors = F)
TB_dataphyto$sampledate = ymd(TB_dataphyto$sampledate)

genus$sampledate = ymd(genus$sampledate)
TB_dataphyto.wide = pivot_wider(TB_dataphyto, names_from = taxa_name, values_from = biovolume_conc)
TB_dataphyto.long = pivot_longer(TB_dataphyto.wide, cols=11:92, names_to="Taxa-name", values_to = "biovolume_conc")

TB_dataphyto %>% 
  group_by(sampledate, taxa_name) %>% 
  mutate(dupe = n() > 1) %>% 
  filter(dupe == TRUE)
TB_dataphyto.wide = TB_dataphyto %>% 
  group_by(sampledate, taxa_name) %>% #deal with duplications
  summarise(CellBioVol = sum(CellBioVol, na.rm = T)) %>% 
  pivot_wider(names_from = Genus, values_from = CellBioVol, values_fill = list(CellBioVol = 0)) #fill NAs with zeros
TB_dataphyto.long = pivot_longer(TB_dataphyto.wide, cols = 2:61, names_to="Genus", values_to = "CellBioVol")
TB_dataphyto.long$CellBioVol[is.na(TB_dataphyto.long$CellBioVol)] = 0











#set NAs to 0s  !!!
write.csv(TB_dataphyto.long, 'data/CleanTBPhytos.csv', row.names = F)


# add true 0s for all bvs for all genus - pivot-wide then zero, then pivot back to long
genus$sampledate = ymd(genus$sampledate)
genus.wide = pivot_wider(genus, names_from = Genus, values_from = CellBioVol)
genus.long = pivot_longer(genus.wide, cols=12:70, names_to="Genus", values_to = "CellBioVol")
try = unlist(genus.long$CellBioVol)
genus.long$cellbv = try

ggplot(subset(genus.long, sampledate=='1997-01-14'), aes(Genus, CellBioVol))+
  geom_point()

###########
genus = read.csv('data/cleanedCellBioVol05202020.csv',stringsAsFactors = F)
######
#hil's info
genus %>% 
  group_by(sampledate, Genus) %>% 
  mutate(dupe = n() > 1) %>% 
  filter(dupe == TRUE)
genus.wide = genus %>% 
  group_by(sampledate, Genus) %>% #deal with duplications
  summarise(CellBioVol = sum(CellBioVol, na.rm = T)) %>% 
  pivot_wider(names_from = Genus, values_from = CellBioVol, values_fill = list(CellBioVol = 0)) #fill NAs with zeros
genus.long = pivot_longer(genus.wide, cols = 2:61, names_to="Genus", values_to = "CellBioVol")
genus.long$CellBioVol[is.na(genus.long$CellBioVol)] = 0


#need to make all of the absence data


TB_zoops= read.csv('data/TB_zoops.csv', stringsAsFactors = F)
TB_zoops$sampledate = mdy(TB_zoops$sampledate)
#need to make all of the absence data
genus$sampledate = ymd(genus$sampledate)
TB_zoops.wide = pivot_wider(TB_zoops, names_from = species_code, values_from = density)
TB_zoops.long = pivot_longer(TB_zoops.wide, cols=5:84, names_to="code", values_to = "density")




try = unlist(TB_zoops.long$density)
TB_zoops.long$density = try
write.csv(TB_zoops.long, 'data/TB_zoops.csv', row.names = F)








#totalbv_TB = read.csv("data/TotalBVs.csv", stringsAsFactors = F)
#totalbv= read.csv("../TotalBVs.csv")
#totalbv$sampledate = ymd(totalbv$sampledate)
TB_data_season %<>% select(lakeid, sampledate, division, taxa_name, genus, biovolume_conc, Season) %>%
  mutate(bv.datePlus1 = sampledate + 1) %>% mutate(bv.dateMinus1 = sampledate - 1)

join_surfchlor <-fuzzy_left_join(join_ice, TB_data_season, by = c("sampledate" = "bv.datePlus1", "sampledate" = "bv.dateMinus1"),
                                 match_fun = list(`<=`, `>=`))

join_surfchlor %<>% rename(sampledate= sampledate.y)


write.csv(join_surfchlor, 'data/joinedTBseasonFull.csv', row.names = F)

class(join_surfchlor$sampledate)

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
TB_data_phytos <- read.csv("data/TB_data_phytos_totals_withNA.csv", 
                                         na = "null")
############################
#genus$sampledate = ymd(genus$sampledate)

#genus.wide = pivot_wider(genus, names_from = Genus, values_from = CellBioVol)

#genus.long = pivot_longer(genus.wide, cols=12:70, names_to="Genus", values_to = "CellBioVol")
TB_data_phytos <- read.csv("data/TB_data_phytos_totals_withNA.csv", na = "null")
TB_data_phytos$sampledate = mdy(TB_data_phytos$sampledate)

#TB_data_phytos %>% 
  group_by(sampledate,division,taxa_name, genus) %>% 
  mutate(dupe = n() > 1) %>% 
  filter(dupe == TRUE)
#TB_data_phytos.wide = TB_data_phytos %>% 
  group_by(sampledate, division,taxa_name, genus) %>% #deal with duplications
  summarise(biovolume_conc = sum(biovolume_conc, na.rm = T)) %>% 
  pivot_wider(names_from = division, values_from = biovolume_conc, values_fill = list(biovolume_conc = 0)) #fill NAs with zeros
#TB_data_phytos.longdiv = pivot_longer(TB_data_phytos.wide, cols = 4:14, names_to="division", values_to = "biovolume_conc")

#write.csv(TB_data_phytos.longdiv, 'data/TBPhytos_div072020.csv', row.names = F)


TB_data_phytos %>% 
  group_by(sampledate,division,taxa_name, genus) %>% 
  mutate(dupe = n() > 1) %>% 
  filter(dupe == TRUE)
TB_data_phytos.wide = TB_data_phytos %>% 
  group_by(sampledate, division,taxa_name, genus) %>% #deal with duplications
  summarise(biovolume_conc = sum(biovolume_conc, na.rm = T)) %>% 
  pivot_wider(names_from = taxa_name, values_from = biovolume_conc, values_fill = list(biovolume_conc = 0)) #fill NAs with zeros
#TB_data_phytos.longtaxa = pivot_longer(TB_data_phytos.wide, cols = 4:86, names_to="taxa_name", values_to = "biovolume_conc")
#TB_data_phytos.longtaxa %>% group_by(division, genus, taxa_name) %>%filter(sum(biovolume_conc, na.rm = T) > 0)
TB_data_phytos.longtaxa = pivot_longer(TB_data_phytos.wide, cols = 4:86, names_to="taxa_name", values_to = "biovolume_conc") %>% 
  group_by(division, genus, taxa_name) %>% 
  filter(sum(biovolume_conc, na.rm = T) > 0)


write.csv(TB_data_phytos.longtaxa, 'data/TBPhytos_taxa2.csv', row.names = F)

TB_data_phytos %>% 
  group_by(sampledate,division,taxa_name, genus) %>% 
  mutate(dupe = n() > 1) %>% 
  filter(dupe == TRUE)
TB_data_phytos.wide = TB_data_phytos %>% 
  group_by(division,taxa_name, genus) %>% #deal with duplications
  summarise_at(vars(sampledate:genus), sum, na.rm = TRUE)
  #summarise(biovolume_conc = sum(biovolume_conc, na.rm = T)) %>% 
  pivot_wider(names_from = taxa_name, values_from = biovolume_conc, values_fill = list(biovolume_conc = 0)) #fill NAs with zeros
TB_data_phytos.longtaxa = pivot_longer(TB_data_phytos.wide, cols = 4:86, names_to="taxa_name", values_to = "biovolume_conc")


summarise_at(vars(cells_per_nu:biomass_conc), sum, na.rm = TRUE)

####################################
genus %>% 
  group_by(sampledate, Genus) %>% 
  mutate(dupe = n() > 1) %>% 
  filter(dupe == TRUE)
genus.wide = genus %>% 
  group_by(sampledate, Genus) %>% #deal with duplications
  summarise(CellBioVol = sum(CellBioVol, na.rm = T)) %>% 
  pivot_wider(names_from = Genus, values_from = CellBioVol, values_fill = list(CellBioVol = 0)) #fill NAs with zeros
genus.long = pivot_longer(genus.wide, cols = 2:61, names_to="Genus", values_to = "CellBioVol")






TB_data_phytos %>% 
  group_by(sampledate, species_name) %>% 
  mutate(dupe = n() > 1) %>% 
  filter(dupe == TRUE)


TB_data_phytos.wide2 = pivot_wider(TB_data_phytos, names_from = division, values_from = biovolume_conc)

TB_data_phytos.long2 = pivot_longer(TB_data_phytos.wide2, cols=11:21, names_to="division", values_to = "biovolume_conc")




write.csv(TB_data_phytos.long, 'data/TBPhytos_probablyno.csv', row.names = F)

write.csv(TB_data_phytos.long2, 'data/TBPhytos_probablydivsion.csv', row.names = F)







###############################

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
TB_zoops %<>% select(sampledate, species_name, density)

TB_zoops %>% 
  group_by(sampledate, species_name) %>% 
  mutate(dupe = n() > 1) %>% 
  filter(dupe == TRUE)
TB_zoops.wide = TB_zoops %>% 
  group_by(sampledate, species_name) %>% #deal with duplications
  summarise(density = sum(density, na.rm = T)) %>% 
  pivot_wider(names_from = species_name, values_from = density, values_fill = list(density = 0)) #fill NAs with zeros
TB_zoops.long = pivot_longer(TB_zoops.wide, cols = 2:80, names_to="species_name", values_to = "density")

genus.long$CellBioVol[is.na(genus.long$CellBioVol)] = 0

write.csv(TB_zoops.long, 'data/TBZoopsTaxaName.csv', row.names = F)
###############

TB.zoops.code= read.csv('data/TB_zoops.csv', stringsAsFactors = F)
TB.zoops.code$sampledate = mdy(TB.zoops.code$sampledate)
#need to make all of the absence data
TB.zoops.code %<>% select(sampledate, species_code, density)

TB.zoops.code %>% 
  group_by(sampledate, species_name) %>% 
  mutate(dupe = n() > 1) %>% 
  filter(dupe == TRUE)
TB_zoops.code.wide = TB.zoops.code %>% 
  group_by(sampledate, species_code) %>% #deal with duplications
  summarise(density = sum(density, na.rm = T)) %>% 
  pivot_wider(names_from = species_code, values_from = density, values_fill = list(density = 0)) #fill NAs with zeros
TB_zoops.long = pivot_longer(TB_zoops.code.wide, cols = 2:81, names_to="species_code", values_to = "density")

write.csv(TB_zoops.long, 'data/TBZoopsCode.csv', row.names = F)
write.csv(TB_zoops.long, 'data/TBZoopsTaxaName.csv', row.names = F)


TB.zoops.code= read.csv('data/TBZoopsCode.csv', stringsAsFactors = F)
TB.zoops.TN= read.csv('data/TBZoopsTaxaName.csv', stringsAsFactors = F)

###############






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

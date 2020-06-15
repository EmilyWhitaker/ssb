#Trout Bog snow and ice over time 

## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin)

##########
data = read.csv('data/phytoplankton_north.csv', stringsAsFactors = F)
data %<>% select(lakeid, year4, sampledate, division, taxa_name,relative_total_biovolume, genus, cells_per_nu,
                 nu_per_ml,	cells_per_ml,	biovolume_conc,	biomass_conc)
data$sampledate = ymd(data$sampledate)


#seperate into TB

TB_data = subset(data, lakeid == "TB")
write.csv(TB_data, 'data/TB_data_phytos.csv', row.names = F)

ggplot(TB_data, sampledate)

# summarize species trends

ggplot(TB_data, aes(as.factor(year4), log(biovolume_conc), fill=taxa_name))+
  geom_boxplot()

genus.ann = TB_data
genus.ann %<>% group_by(year4, taxa_name) %>%
  summarize(med.bv = median(log(biovolume_conc), na.rm=T))

genus.ann$med.bv[is.na(genus.ann$med.bv)] <- 0

tile=genus.ann
tile = arrange(tile,taxa_name)
tile$Genus = factor(tile$taxa_name, levels = rev(unique(tile$taxa_name)), ordered=TRUE)

ggplot(tile, aes(x=year4,y=taxa_name,fill=med.bv))+
  geom_tile(size=0.1,na.rm=T)+
  scale_fill_viridis_c(direction=-1)+
  theme_classic()

#SP data

SP_data <- subset(data, lakeid== "SP")


ggplot(SP_data, aes(as.factor(year4), log(biovolume_conc), fill=taxa_name))+
  geom_boxplot()

genus.ann.sp = SP_data
genus.ann.sp %<>% group_by(year4, taxa_name) %>%
  summarize(med.bv = median(log(biovolume_conc), na.rm=T))

genus.ann.sp$med.bv[is.na(genus.ann.sp$med.bv)] <- 0

tile2=genus.ann.sp
tile2 = arrange(tile,taxa_name)
tile2$Genus = factor(tile$taxa_name, levels = rev(unique(tile$taxa_name)), ordered=TRUE)

ggplot(tile2, aes(x=year4,y=taxa_name,fill=med.bv))+
  geom_tile(size=0.1,na.rm=T)+
  scale_fill_viridis_c(direction=-1)+
  theme_classic()


write.csv(SP_data, 'data/SP_data_phytos.csv', row.names = F)


# pull out genus-specific biovolumes
# skip ahead to clean file
genus = subset(data, Genus != "TotalBiovolume")







#make Holly's graph


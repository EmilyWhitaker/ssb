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

ggplot(subset(genus,year<2000), aes(as.factor(year4), log(biovolume_conc), fill=taxa_name))+
  geom_boxplot(aes(group=taxa_name))+
  geom_line(data=subset(genus.ann, year<2000), aes(as.factor(year4), med.bv, color=taxa_name, group=taxa_name))

tile=genus.ann
tile = arrange(tile,Genus)
tile$Genus = factor(tile$Genus, levels = rev(unique(tile$Genus)), ordered=TRUE)

ggplot(tile, aes(x=year,y=Genus,fill=med.bv))+
  geom_tile(size=0.1,na.rm=T)+
  scale_fill_viridis_c(direction=-1)+
  theme_classic()

#SP data

SP_data <- subset(data, lakeid== "SP")
write.csv(SP_data, 'data/SP_data_phytos.csv', row.names = F)


# pull out genus-specific biovolumes
# skip ahead to clean file
genus = subset(data, Genus != "TotalBiovolume")







#make Holly's graph


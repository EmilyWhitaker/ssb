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

data = read.csv('data/phytoplankton_north.csv', stringsAsFactors = F)
data %<>% select(lakeid, year4, sampledate, division, taxa_name,relative_total_biovolume, genus, cells_per_nu,
                 nu_per_ml,	cells_per_ml,	biovolume_conc,	biomass_conc)
data$sampledate = ymd(data$sampledate)
TB_data = subset(data, lakeid == "TB")
write.csv(TB_data, 'data/TB_data_phytos.csv', row.names = F)

ggplot(TB_data, sampledate)

# summarize species trends

TB_data_season = read.csv('data/TB_data_phytos_totals_season.csv', stringsAsFactors = F)
TB_data_season %<>% select(lakeid, year4, sampledate, division, taxa_name,relative_total_biovolume, genus, cells_per_nu,
                 nu_per_ml,	cells_per_ml,	biovolume_conc,	biomass_conc, Season)
TB_data_season$sampledate = mdy(TB_data_season$sampledate)
TB_data_season_total = subset(TB_data_season, division == "Total")

ggplot((TB_data_season_total), aes(sampledate, biovolume_conc))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  #geom_point()+
  scale_color_viridis_c(option="viridis")+
  geom_smooth(method=lm,se=T)+
  #scale_color_brewer(palette = 'Paired')+
  facet_wrap(~Season)+
  labs(title="Trout Bog Seasonal Trends", x= "Year", y= "Biovolume")+
  theme_classic()

ggplot(TB_data, aes(as.factor(year4), log(biovolume_conc), fill=taxa_name))+
  geom_boxplot()

genus.ann = TB_data
genus.ann %<>% group_by(year4, taxa_name) %>%
  summarize(med.bv = median(log(biovolume_conc), na.rm=T))

genus.ann$med.bv[is.na(genus.ann$med.bv)] <- 0

tile=genus.ann
tile = arrange(tile,taxa_name)
tile$TB_data = factor(tile$taxa_name, levels = rev(unique(tile$taxa_name)), ordered=TRUE)

ggplot(tile, aes(x=year4,y=taxa_name,fill=med.bv))+
  geom_tile(size=0.1,na.rm=T)+
  scale_fill_viridis_c(direction=-1)+
  theme_classic()


#SP data

SP_data <- subset(data, lakeid== "SP")


ggplot(SP_data, aes(as.factor(year4), log(biovolume_conc), fill=taxa_name))+
  geom_boxplot()

genus.ann = SP_data
genus.ann %<>% group_by(year4, genus) %>%
  summarize(med.bv = median(log(biovolume_conc), na.rm=T))

genus.ann$med.bv[is.na(genus.ann$med.bv)] <- 0

tile=genus.ann
tile = arrange(tile,genus)
tile$SP_data = factor(tile$genus, levels = rev(unique(tile$genus)), ordered=TRUE)

ggplot(tile, aes(x=year4,y=genus,fill=med.bv))+
  geom_tile(size=0.1,na.rm=T)+
  scale_fill_viridis_c(direction=-1)+
  theme_classic()


# pull out genus-specific biovolumes
TB_data_full = read.csv('data/TB_data_phytos_totals.csv', stringsAsFactors = F)
TB_data_full %<>% select(year4, sampledate, division, taxa_name,relative_total_biovolume, genus, cells_per_nu,
                 nu_per_ml,	cells_per_ml,	biovolume_conc,	biomass_conc)
TB_data_full$sampledate = mdy(TB_data_full$sampledate)

class(TB_data_full$sampledate)

TB_data_full_total = subset(TB_data_full, division == "Total")


ggplot(TB_data_full, aes(x=sampledate,y=division,fill=med.bv))+
  geom_tile(size=0.1,na.rm=T)+
  scale_fill_viridis_c(direction=-1)+
  theme_classic()



SP_data_season = read.csv('data/SP_data_phytos_totals_season.csv', stringsAsFactors = F)
SP_data_season %<>% select(lakeid, year4, sampledate, division, taxa_name,relative_total_biovolume, genus, cells_per_nu,
                           nu_per_ml,	cells_per_ml,	biovolume_conc,	biomass_conc, Season)
SP_data_season$sampledate = mdy(SP_data_season$sampledate)
SP_data_season_total = subset(SP_data_season, division == "Total")

ggplot((SP_data_season_total), aes(sampledate, biovolume_conc))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  #geom_point()+
  scale_color_viridis_c(option="viridis")+
  geom_smooth(method=lm,se=T)+
  #scale_color_brewer(palette = 'Paired')+
  facet_wrap(~Season)+
  labs(title="Sparkling Lake Seasonal Trends", x= "Year", y= "Biovolume")+
  theme_classic()


#value by year 
ggplot(TB_data_full, aes(sampledate, log(biovolume_conc)))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_smooth()+
  theme_classic()

ggplot(TB_data_full_total, aes(sampledate, log(biovolume_conc)))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  scale_color_viridis_c(option="viridis")+
  geom_smooth()+
  #scale_color_brewer(palette = 'Paired')+
  #facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))+
  theme_classic()+
  labs(title="Trout Bog Totals")+
  labs(x='Year')


gg <- ggplot(subset(totals.new, Genus %in%gen.ndmi, aes(x=sampledate, y=log(CellBioVol)))) + 
  geom_point(aes(col=chlor.int))+ 
  geom_smooth()+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))
plot(gg)

#######
#Sparkling Lake info
SP_data_full = read.csv('data/SP_data_phytos_totals.csv', stringsAsFactors = F)
SP_data_full %<>% select(year4, sampledate, division, taxa_name,relative_total_biovolume, genus, cells_per_nu,
                         nu_per_ml,	cells_per_ml,	biovolume_conc,	biomass_conc)
SP_data_full$sampledate = mdy(SP_data_full$sampledate)

SP_data_full_total = subset(SP_data_full, division == "Total")


ggplot(SP_data_full_total, aes(sampledate, log(biovolume_conc)))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  scale_color_viridis_c(option="viridis")+
  geom_smooth()+
  #scale_color_brewer(palette = 'Paired')+
  #facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))+
  theme_classic()+
  labs(title="Sparkling Lake Totals")+
  labs(x='Year')


#####
#Pair this DOWN

SP_data_season

spgenus=unique(SP_data_season$genus)
write.csv(spgenus, 'data/spgenus.csv', row.names = F)


#######

unique(TB_data_season$taxa_name)




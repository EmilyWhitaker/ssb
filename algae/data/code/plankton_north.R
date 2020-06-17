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




########
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

chl = read.csv('data/chloro_all.csv',stringsAsFactors = F)
chl %<>% subset(lakeid == "SP" & depth == 0) %>%
  select(year4, daynum, sampledate, chlor)
chl$sampledate =  ymd(chl$sampledate)

#Inegrated Chems
intchems = read.csv("data/SPFullChem.csv", stringsAsFactors = F)
intchems$sampledate = mdy(intchems$sampledate)
intchems %<>% subset(lakeid == "SP")
intchems$frlight[intchems$frlight=="1"] <- NA #one iceon point with no light point to calc frlight against
intchems

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

#Cleaning col names
joinedTBseasonFull = read.csv('data/joinedTBseasonFull.csv', stringsAsFactors = F)

joinedTBseasonFull %<>% rename(chlor.int = intchlor)
joinedTBseasonFull %<>% rename(chlor.surf = chlor.y)

fulldatasetclean05202020_2= fulldatasetclean05202020 %>% select(sampledate.x:chlor.surf, year4,
                                                                avsnow:blueice,Genus.y:CellBioVol.y)

fulldatasetclean05202020_2%<>% rename(Genus = Genus.y)
fulldatasetclean05202020_2%<>% rename(CellBioVol = CellBioVol.y)
fulldatasetclean05202020_2%<>% rename(sampledate = sampledate.x)

fulldatasetclean05202020_2$avsnow[is.na(fulldatasetclean05202020_2$avsnow)]=0
fulldatasetclean05202020_2$totice[is.na(fulldatasetclean05202020_2$totice)]=0
fulldatasetclean05202020_2$blueice[is.na(fulldatasetclean05202020_2$blueice)]=0
fulldatasetclean05202020_2$whiteice[is.na(fulldatasetclean05202020_2$whiteice)]=0

Genera = read.csv("data/CleanBiovolumes05202020", stringsAsFactors = F)
Genera$sampledate = ymd(Genera$sampledate)

fulldatasetclean05202020 = full_join(join_surfchlor, Genera, by=c('sampledate'))
write.csv(fulldatasetclean05202020, 'data/fulldatasetclean05202020.csv', row.names = F)

#####
#combine SP data 


SP_data_season %<>% select(lakeid, sampledate, division, taxa_name, genus, biovolume_conc, Season) %>%
  mutate(bv.datePlus1 = sampledate + 1) %>% mutate(bv.dateMinus1 = sampledate - 1)

join_surfchlor_SP <-fuzzy_left_join(join_ice, SP_data_season, by = c("sampledate" = "bv.datePlus1", "sampledate" = "bv.dateMinus1"),
                                 match_fun = list(`<=`, `>=`))

join_surfchlor_SP %<>% rename(sampledate= sampledate.y)


write.csv(join_surfchlor_SP, 'data/joinedSPseasonFull.csv', row.names = F)

class(join_surfchlor_SP$sampledate)

write.csv(SP_data_season_total, 'data/SP_data_season_total.csv', row.names = F)

#############
#looking at SP full seasonal dataset 

SPdataset = read.csv('data/joinedSPseasonFull_06172020.csv', stringsAsFactors = F)

SPdataset %<>% rename(chlor.int = chlor.x)
SPdataset %<>% rename(chlor.surf = chlor.y)
SPdataset$avsnow[is.na(SPdataset$avsnow)]=0
SPdataset$totice[is.na(SPdataset$totice)]=0
SPdataset$blueice[is.na(SPdataset$blueice)]=0
SPdataset$whiteice[is.na(SPdataset$whiteice)]=0
SPdataset= SPdataset %>% select(sampledate.x,	wtemp,	o2,	o2sat,	cond,	frlight,	chlor.int,	phaeo,	ph,	phair,	alk,	dic,	tic,	
                                doc,	toc,	no3no2,	no2, nh4,	totnf,	totnuf,	totpf,	totpuf,	drsif,	brsif,	brsiuf,	tpm,	cl,
                                so4,	ca,	mg,	na,	k,	fe,	mn,	chlor.surf,	avsnow,	totice,	whiteice,	
                                blueice,	division,	taxa_name,	genus,	biovolume_conc,	Season)		
SPdataset %<>% rename(sampledate = sampledate.x)
SPdataset %<>% rename(biovolume = biovolume_conc)                                
SPdataset$sampledate = mdy(SPdataset$sampledate)

write.csv(SPdataset, 'data/cleanSPdataset.csv', row.names = F)

#line graph of divisions and total with biovolume over time 

#SPdataset.long = pivot_longer(SPdataset, cols=c("division",	"taxa_name","genus",	"biovolume"), names_to="variable", values_to = "value")

ggplot(data.total.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')


#cutting dates

SPdataset.dates= subset(SPdataset$sampledate>2009)


ggplot(SPdataset, aes(sampledate,biovolume, color=division))+
  geom_point()+
  geom_smooth(aes(group=division))+
  geom_line(aes(group=division))+
  xlim(2010-01-12)




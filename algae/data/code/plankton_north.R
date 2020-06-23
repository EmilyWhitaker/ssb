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


write.csv(TB_data_full_total,'data/TB_data_full_total.csv', row.names = F)



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
SPdataset.dates = read.csv('data/cleanSPdataset_limiteddates.csv', stringsAsFactors = F)
SPdataset.dates$sampledate = mdy(SPdataset.dates$sampledate)

SPdataset.dates.nottotals= subset(SPdataset.dates, division != "Total")

SPdataset.dates.clean = read.csv('data/cleanSPdataset_limiteddates_limiteddiv.csv', stringsAsFactors = F)
SPdataset.dates.clean$sampledate = mdy(SPdataset.dates.clean$sampledate)

SPdataset.dates.clean.nottotals= subset(SPdataset.dates.clean, division != "Total")

ggplot(SPdataset.dates.clean.nottotals, aes(sampledate, log(biovolume), color=division))+
  geom_point()+
  #geom_smooth(aes(group=division))+
  geom_line(aes(group=division))+
  facet_wrap('Season')+
  theme_classic()

ggplot(SPdataset.dates.clean.nottotals, aes(sampledate, biovolume, color=division))+
  geom_bar(position="dodge", stat="division")+
  #geom_smooth(aes(group=division))+
  facet_wrap('Season')+
  theme_classic()+
  labs(title="Sparkling Lake Biovolume")+
  labs(x='Year', y= 'Biovolume')+

specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

# Stacked
TBdataset.dates.nottotals= subset(TB_data_season, division != "Total")


ggplot(TBdataset.dates.nottotals, aes(fill=division, y=biovolume_conc, x=sampledate)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  labs(title="Trout Bog Biovolume")+
  labs(x='Year', y= 'Biovolume')
  
  




SPdataset.dates.clean.iceon= subset(SPdataset.dates.clean, Season == 0)

summer.data.total.long = pivot_longer(SPdataset.dates.clean.iceon, cols=c("wtemp","o2","avsnow","totice","whiteice","blueice", "chlor.int",
                                                           "frlight","chlor.surf"), names_to="variable", values_to = "value")

ggplot(summer.data.total.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')


#divisions 

unique(SPdataset.dates.clean.iceon$division)

gen.main.total = c("Armored Dinoflagellate","Naked Dinoflagellate","Limnothrix","Microcystis","Lindavia",
                   "Cryptomonad", "Asterionella", "Dinobryon", "Fragilaria","Cocconeis",
                   "Flagellated Green", "Cf. Komvophoron / Trichormus","Cyanobacteria","Elakatothrix","Mallomonas",
                   "Segmented Green", "Peanut","Other")
gen.ndmi=c("Asterionella")

ggplot((subset(totals, Genus %in%gen.ndmi)), aes(sampledate,log(CellBioVol)))+
  geom_point(aes(col=chlor.surf), size=2)+
  geom_smooth(method=lm,se=T)+
  labs(title="Peanut Integrated Chlorophyll")+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))
#ntl=c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Cyanophyta", "Haptophyta",
#"Pyrrhophyta", "Total", "empty", "Chrysophyta", "Euglenophyta", "Miscellaneous")

SPdataset.dates.clean.iceon$year= year(SPdataset.dates.clean.iceon$sampledate)
SPdataset.dates.clean.iceon.nototals= subset(SPdataset.dates.clean.iceon, division != "Total")
#ngt$year= year(ngt$sampledate)

gen.ndmi=c("Chlorophyta")

ggplot((subset(SPdataset.dates.clean.iceon)), aes(whiteice, log(biovolume)))+
  geom_point(aes(col=division), size=2)+
 # geom_smooth()+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='White Ice (cm)', y= 'Log Biovolume')





ggplot(SPdataset.dates.clean.nottotals, aes(fill=division, y=biovolume, x=whiteice)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  labs(title="Sparkling Lake Biovolume in Relation to White Ice")+
  labs(x='White ice', y= 'Biovolume')

ggplot(SPdataset.dates.clean.iceon.nototals, aes(fill=division, y=biovolume, x=totice)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  labs(title="Sparkling Lake Biovolume in Relation to Total Ice")+
  labs(x='Total Ice', y= 'Biovolume')

SPdataset.dates.clean.iceon= subset(SPdataset.dates.clean, Season == 0)
SPdataset.dates.clean.iceoff= subset(SPdataset.dates.nottotals, Season == 1)

#our two seasons: SPdataset.dates.clean.iceon.nototals, SPdataset.dates.clean.iceoff
# vars: wtemp,	o2,	o2sat,	cond,	frlight,	chlor.int,	phaeo,	ph,	phair,	alk,	dic,	tic,	
#doc,	toc,	no3no2,	no2, nh4,	totnf,	totnuf,	totpf,	totpuf,	drsif,	brsif,	brsiuf,	tpm,	cl,
#so4,	ca,	mg,	na,	k,	fe,	mn,	chlor.surf,	avsnow,	totice,	whiteice,	
#blueice

ggplot(SPdataset.dates.clean.iceon.nototals, aes(dic, biovolume))+
  geom_point(aes(col=division), size=2)+
  # geom_smooth()+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='dic', y= 'Biovolume')+
  labs(title="Sparkling Lake Biovolume in Relation to dic Ice On")


ggplot(SPdataset.dates.clean.iceon.nototals, aes(fill=division, y=biovolume, x=doc)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  labs(title="Sparkling Lake Biovolume in Relation to Dissolved Organic Carbon Ice On")+
  labs(x='Dissolved Organic Carbon', y= 'Biovolume')+
  facet_wrap('division', scales = "free")
 

# facet_wrap("Season", scales = "free")
#need to redo cond/frlight/dic/doc/no3no2/
#-- scales ,, 
#define phaeo (phaeopigment)/phair,
#Nitrite/brsiuf -N/A

ggplot(SPdataset.dates.clean.iceoff, aes(fill=division, y=biovolume, x=chlor.int)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  labs(title="Sparkling Lake Biovolume in Relation to Integrated Chlorophyll Ice Off")+
  labs(x='Integrated Chlorophyll', y= 'Biovolume')#
#  facet_wrap(scales = "free")

#need to redo: wtemp,o2,	o2sat,	chlor.int,	phaeo,	alk,	dic,	tic,	
#doc,	toc,	no3no2,	no2, nh4,	totnf,	totnuf,	totpf,	totpuf,	drsif,	
#so4,	ca,	na,	fe,

#brsif/brsiuf-- nA
SPdataset.dates.clean.iceoff_nozeros= subset(SPdataset.dates.clean.iceoff, doc>=0)
SPdataset.dates.clean.iceoff_nozeros= subset(SPdataset.dates.clean.iceoff, toc>=0)
SPdataset.dates.clean.iceoff_nozeros= subset(SPdataset.dates.clean.iceoff, no3no2>=0)
SPdataset.dates.clean.iceoff_nozeros= subset(SPdataset.dates.clean.iceoff, nh4>=0)
SPdataset.dates.clean.iceoff_nozeros= subset(SPdataset.dates.clean.iceoff_nozeros, toc>=0)
  
  
ggplot(SPdataset.dates.clean.iceoff_nozeros, aes(fill=division, y=biovolume, x=nh4)) + 
  geom_bar(stat="identity", width = .5)+
  theme_classic()+
  labs(title="Sparkling Lake Biovolume in Relation to o2sat Ice Off")+
  labs(x='o2sat', y= 'Biovolume')+
  facet_wrap('division', scales = 'free')


ggplot(SPdataset.dates.clean.iceoff_nozeros, aes(ca, biovolume))+
  geom_point(aes(col=division), size=2)+
  # geom_smooth()+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  facet_wrap('division', scales = 'free')+
  labs(x='ca', y= 'Biovolume')+
  labs(title="Sparkling Lake Biovolume in Relation to Ca Ice Off")

#need to redo: ,	phaeo,		
#	, nh4,	totnf,	totnuf,	totpf,	totpuf,	drsif,	
#so4,	ca,	na,	

fe,

#phair whatdoes it stand 4
#doc/toc/no3no2/nh4 remove values less than 0-- redo doc
#cl might want to redo

#no2/brsif-NA
############

install.packages("devtools")
devtools::install_github("joelleforestier/PridePalettes")
library(PridePalettes)


+
  facet_wrap("Season")

aes(col=frlight), size=2


#########################
#Can you explore the relationship between biovolume and chlorophyll more? Like BV vs Chl at different depths.
#Yes, try
#integrated BV vs Chl at 1m,
#integrated BV vs Chl at 4m, etc…
#If you look at the chlorophyll data, you’ll see where the peaks are and how that changes seasonally. 
#There might be stronger correlations at certain depths.

Chloro1 = read.csv('data/SPChloro2010-2015.csv', stringsAsFactors = F)
Chloro1 %<>% select(year4, sampledate, depth, chlor)
Chloro1$sampledate = ymd(Chloro1$sampledate)
write.csv(Chloro1, 'data/SPChloro2010.Clean.csv', row.names = F)


Chloro2 = read.csv('data/SPChloro2010.Clean.withbv.csv', stringsAsFactors = F)
Chloro2$sampledate = mdy(Chloro2$sampledate)
Chloro2


Chloro3 = read.csv('data/SPChloro2010.Clean.withbv.nonas.csv', stringsAsFactors = F)
Chloro3$sampledate = mdy(Chloro3$sampledate)
Chloro3$month=month(Chloro3$sampledate)


Chloro3


ggplot(Chloro2, aes(sampledate, chlor))+
  geom_point(aes(col=depth), size=2)+
  # geom_smooth()+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='Sample Date', y= 'Chloro')

ggplot(Chloro2, aes(chlor, biovolume_conc))+
  geom_point(aes(col=depth), size=2)+
  # geom_smooth()+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='chlor', y= 'biovolume_conc')


Chloro3_0m <- subset(Chloro3, depth ==0)
Chloro3_3m <- subset(Chloro3, depth ==3)
Chloro3_5m <- subset(Chloro3, depth ==5)
Chloro3_8m <- subset(Chloro3, depth ==8)
Chloro3_10m <- subset(Chloro3, depth ==10)
Chloro3_12m <- subset(Chloro3, depth ==12)
Chloro3_15m <- subset(Chloro3, depth ==15)
Chloro3_18m <- subset(Chloro3, depth ==18)

Chloro2_0m

ggplot(Chloro3_0m, aes(chlor, biovolume_conc, colour = month)) +
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  facet_wrap("Season", scales = 'free')



a<-ggplot(Chloro3_5m, aes(chlor, biovolume_conc))+
  geom_point(aes(col=month), size=2)+
  geom_smooth(method = 'lm')+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='chlor', y= 'biovolume_conc')+
  facet_wrap("month", scales = 'free')
a+ scale_color_gradientn(colours = rainbow(10))


b<- ggplot(Chloro3_18m, aes(chlor, biovolume_conc))+
  geom_point(aes(col=month), size=2)+
  geom_smooth(method = 'lm')+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='chlor', y= 'biovolume_conc')+
  labs(x='Chlorophyll at Depth', y= 'Biovolume')+
  facet_wrap('month', scales = 'free')+
  labs(title="18 meters")
b+ scale_color_gradientn(colours = rainbow(10))

c <- ggplot(Chloro3_3m, aes(chlor, biovolume_conc))+
  geom_point(aes(col=month), size=2)+
  geom_smooth(method = 'lm',s)+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='chlor', y= 'biovolume_conc')+
  facet_wrap('month', scales = 'free')+
  labs(title="3 meters")
c+ scale_color_gradientn(colours = rainbow(10))

d <- ggplot(Chloro3_0m, aes(chlor, biovolume_conc))+
  geom_point(aes(col=month), size=2)+
  geom_smooth(method = 'lm')+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='chlor', y= 'biovolume_conc')+
  facet_wrap('month', scales = 'free')+
  labs(title="0 meters")
d+ scale_color_gradientn(colours = rainbow(10))



ggplot(Chloro2_3m, aes(chlor, biovolume_conc))+
  geom_point(aes(col=depth), size=2)+
  # geom_smooth()+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='chlor', y= 'biovolume_conc')

ggplot(Chloro3_5m, aes(chlor, biovolume_conc))+
  geom_point(aes(col=season), size=2)+
  geom_smooth(method = 'lm')+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='chlor', y= 'biovolume_conc')+
  facet_wrap("Season")

h<-ggplot(Chloro2_8m, aes(chlor, biovolume_conc))+
  geom_point(aes(col=depth), size=2)+
  # geom_smooth()+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='chlor', y= 'biovolume_conc')
h+ stat_cor(method = "pearson", label.x = 3, label.y = 30)

ggscatter(Chloro2_8m, x = "chlor", y = "biovolume_conc", palette = "jco",
                add = "reg.line", conf.int = TRUE)



ggplot(Chloro2_10m, aes(chlor, biovolume_conc))+
  geom_point(aes(col=depth), size=2)+
  # geom_smooth()+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='chlor', y= 'biovolume_conc')

ggplot(Chloro2_12m, aes(chlor, biovolume_conc))+
  geom_point(aes(col=depth), size=2)+
  # geom_smooth()+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='chlor', y= 'biovolume_conc')

ggplot(Chloro2_15m, aes(chlor, biovolume_conc))+
  geom_point(aes(col=Season), size=2)+
  # geom_smooth()+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='chlor', y= 'biovolume_conc')

ggplot(Chloro2_18m, aes(chlor, biovolume_conc))+
  geom_point(aes(col=Season), size=2)+
  # geom_smooth()+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='chlor', y= 'biovolume_conc')


install.packages("ggcorrplot")
library(ggcorrplot)

# Compute a correlation matrix
#data(mtcars)
#corr <- round(cor(mtcars), 1)
#head(corr[, 1:6])

# Compute a matrix of correlation p-values
#p.mat <- cor_pmat(mtcars)
#head(p.mat[, 1:4])

# Visualize the correlation matrix
#ggcorrplot(corr)

Chloro3_short <- Chloro3[c(4,5,10,12)]
head(Chloro3_short)
cor(Chloro3_short)
corrplot(Chloro3_short)
ggcorrplot(Chloro3_short)
ggcorrplot(Chloro0_short, method = "circle")



Chloro0_short <- Chloro3_0m[c(4,5,10,12)]
corrdepth0 <- round(cor(Chloro3_0m),[c(4,5,10,12)])
corrdepth3 <- round(cor(Chloro2_3m), 1)
corrdepth5 <- round(cor(Chloro2_5m), 1)
corrdepth8 <- round(cor(Chloro2_8m), 1)
corrdepth10 <- round(cor(Chloro2_10m), 1)
corrdepth12 <- round(cor(Chloro2_12m), 1)
corrdepth15 <- round(cor(Chloro2_15m), 1)
corrdepth18 <- round(cor(Chloro2_18m), 1)

Chloro2_0m <- subset(Chloro2, depth ==0)
Chloro2_3m <- subset(Chloro2, depth ==3)
Chloro2_5m <- subset(Chloro2, depth ==5)
Chloro2_8m <- subset(Chloro2, depth ==8)
Chloro2_10m <- subset(Chloro2, depth ==10)
Chloro2_12m <- subset(Chloro2, depth ==12)
Chloro2_15m <- subset(Chloro2, depth ==15)
Chloro2_18m

res2 <-cor.test(Chloro3_short$depth, Chloro3_short$chlor, Chloro3_short$biovolume_conc, method = "spearman")
res2

######
#Does that line up with what we know about the depth of chlorophyll maxima in Sparkling?

ChloroCheck = read.csv('data/SPChloro2010.Clean.withbv.csv', stringsAsFactors = F)

ChloroCheck$sampledate = mdy(ChloroCheck$sampledate)
ChloroCheck$month=month(ChloroCheck$sampledate)
ChloroCheck= ChloroCheck %>% select(year,	sampledate,	depth,	chlor, biovolume_conc, Season, month)		

ChloroCheck.0m <- subset(ChloroCheck, depth ==0)
ChloroCheck.jan <- subset(ChloroCheck, month ==1)
ChloroCheck.feb <- subset(ChloroCheck, month ==2)
ChloroCheck.march <- subset(ChloroCheck, month ==3)
ChloroCheck.april <- subset(ChloroCheck, month ==4)
ChloroCheck.may <- subset(ChloroCheck, month ==5)
ChloroCheck.june <- subset(ChloroCheck, month ==6)
ChloroCheck.july<- subset(ChloroCheck, month ==7)
ChloroCheck.august <- subset(ChloroCheck, month ==8)
ChloroCheck.sept <- subset(ChloroCheck, month ==9)
ChloroCheck.oct<- subset(ChloroCheck, month ==10)
ChloroCheck.nov <- subset(ChloroCheck, month ==11)


max(ChloroCheck.jan$chlor) #15.3 

ChloroCheck
d <- ggplot(ChloroCheck.nov, aes(chlor, depth))+
  geom_point(aes(col=depth), size=2)+
  #geom_smooth(method = 'lm')+
  #geom_line()+  
  theme_classic()+
  labs(x='chlor', y= 'depth')+
  labs(title="November")+
  ylim(20,0)+
  xlim(0,NA)
d+ scale_color_gradientn(colours = rainbow(10))


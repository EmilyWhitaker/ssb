## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz 
library(lubridate) # dealing with dates

#=========
# skip ahead
chl = read.csv('data/chloro_all.csv',stringsAsFactors = F)
chl %<>% subset(lakeid == "SP" & depth == 0) %>%
  select(year4, daynum, sampledate, chlor)
chl$sampledate =  ymd(chl$sampledate)

ice = read.csv('data/snowicedepth.csv',stringsAsFactors = F)
ice %<>% subset(lakeid == "SP") %>%
  select(year4, daynum, sampledate, avsnow, totice, whiteice, blueice) %>%
  mutate(perwhiteice=(whiteice/totice)*100, 
         perblueice=(blueice/totice)*100)
ice$sampledate = ymd(ice$sampledate)

abiotic = full_join(chl, ice, by=c('year4',"daynum",'sampledate'))
abiotic %<>% rename(year = year4)

new = read.csv('data/newfile.csv', stringsAsFactors = F)
new %<>% select(sampledate, light)
new$sampledate = mdy(new$sampledate)
new$year = year(new$sampledate)
new$daynum = yday(new$sampledate)

abiotic = full_join(abiotic, new, by=c("year","daynum","sampledate"))

# deal with algae dataset
algae = read.csv('data/biovolumereal.csv',stringsAsFactors = F)

algae %<>% select(sampledate, Genus, CellBioVol)

algae$sampledate = mdy(algae$sampledate)
algae$daynum = yday(algae$sampledate)
algae$year = year(algae$sampledate)

data = full_join(abiotic, algae, by=c("year","daynum","sampledate"))

data %<>% subset(year > 1996)

write.csv(data, 'data/clean_algae_abiotic_03032020.csv',row.names = F)


#======================
data = read.csv('data/clean_algae_abiotic_03032020.csv',stringsAsFactors = F)

# pull out total biovolumes
totals = subset(data, Genus == "TotalBiovolume")
totals$chlor = abs(totals$chlor)

# pull out genus-specific biovolumes
# skip ahead to clean file
genus = subset(data, Genus != "TotalBiovolume")

#========
# look at total biovolume over time

totals %<>% select(-Genus)
totals$log.chlor = log(totals$chlor)
totals$log.bv = log(totals$CellBioVol)
totals.long = pivot_longer(totals, cols=c("chlor","log.chlor","avsnow","totice","whiteice","blueice","perwhiteice","perblueice",
                                          "light","CellBioVol","log.bv"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(year, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')

# look at winter total biovolume
totals.long$month = month(totals.long$sampledate)

winter = subset(totals.long, month < 4)

#only pull out winter sampling
ggplot(winter, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')

#==============
# skip ahead
# look at genus and correct spelling mistakes, etc.
#SpellCheck

genus[genus=="unID cyanobacteria (colony)"] <- NA
genus[genus=="?10"] <- NA
genus[genus=="Cyclotella"] <- 'Lindavia comensis'
genus[genus=="Cyclotella ocellata"] <- 'Lindavia comensis'
genus[genus=="Cyclotella distinguenda"] <- 'Lindavia comensis'
genus[genus=="?11"] <- NA
genus[genus=="Microplastic - blue filament"] <- NA
genus[genus=="Microcyctis"] <- 'Microcystis'
genus[genus=="Microcystis (small)"] <- 'Microcystis'
genus[genus=="Microcystis (large)"] <- 'Microcystis'
genus[genus=="Thin Rod"] <- NA
genus[genus=="Lindavia comensis"] <- 'Lindavia'
genus[genus=="Lindavia affinis"] <- 'Lindavia'
genus[genus=="Lindavia "] <- 'Lindavia'
genus[genus=="Lindavia (Lindavia affinis)"] <- 'Lindavia'
genus[genus=="Lindavia cf. bodanica"] <- 'Lindavia'
#genus[Genus=="Lindavia"]== genus[BioVolume== '44.42277385']
genus[genus=="Cf. Cryptomonad (NEW CATEGORY)"] <- 'Cf. Cryptomonad'
genus[genus=="Segmented green"] <- 'Segmented Green'
genus[genus=="Mallomonas c"] <- 'Mallomonas'
genus[genus=="Mallomonas (colony)"] <- 'Mallomonas'
genus[genus=="Mallomonas Colony"] <- 'Mallomonas'
genus[genus=="Lindavia comensis (C. Comensis))"] <- 'Lindavia'
genus[genus=="Flagellated green"] <- 'Flagellated Green'
genus[genus=="Cyanobacterium diachloros"] <- 'Cyanobacteria'
genus[genus=="Cyanobacteria diachloros"] <- 'Cyanobacteria'
genus[genus=="Cyanobacteria diachlorus"] <- 'Cyanobacteria'
genus[genus=="unID colonial Cyanobacteria"] <- 'Cyanobacteria'
genus[genus=="unID Cyanobacteria"] <- 'Cyanobacteria'
genus[genus=="unID cyanobacteria"] <- 'Cyanobacteria'
genus[genus=="Thin rod"] <- NA
genus[genus=="Lindavia comensis (C. Comensis)"] <- 'Lindavia'
genus[genus=="Aulacoseira cf. subarctica"] <- 'Aulacoseira'
genus[genus=="Aulacoseira cf. ambigua"] <- 'Aulacoseira'
genus[genus=="Aulacoseira cf. islandica"] <- 'Aulacoseira'
genus[genus=="Chroococcus cf. kidneys"] <- 'Chroococcus'
genus[genus=="Chroococcus "] <- 'Chroococcus'
genus[genus=="Cosmarium (Croococcus cf. kidneys)"] <- 'Chroococcus'
genus[genus=="Croococcus cf. kidneys"] <- 'Chroococcus'
genus[genus=="Small chroococcus"] <- 'Small Chroococcus'
genus[genus=="Dinobryon internal organelles ONLY"] <- NA
genus[genus=="Cf. Fragilariforma constricta"] <- NA
genus[genus=="?17"] <- 'Chroococcus cf. kidneys'
genus[genus=="Fragilaria crototensis"] <- 'Fragilaria'
genus[genus=="Fragilaria "] <- 'Fragilaria'
genus[genus=="Fragilaria intermedia"] <- 'Fragilaria'
genus[genus=="Fragilaria tenera"] <- 'Fragilaria'
genus[genus=="Fragilaria crotonensis"] <- 'Fragilaria'
genus[genus=="Planktothrix cells"] <- 'Planktothrix'
genus[genus=="Planktothrix strand"] <- NA
genus[genus=="cf. Planktolyngabia"] <- 'Planktolyngabia'
genus[genus=="NA"] <- NA
genus[genus=="Mallomonas colony"] <- 'Mallomonas'
genus[genus=="Discostella "] <- 'Discostella' #one bv is zero need to change to 350.7702735
genus[genus=="Discotella"] <- 'Discostella'
genus[genus=="Small Cocconeis"] <- 'Cocconeis'
genus[genus=="Small cocconeis"] <- 'Cocconeis'
genus[genus=="unID Cocconeis"] <- 'Cocconeis'
genus[genus=="Small cocconies"] <- 'Cocconeis'
genus[genus=="Hydrococcus"] <- NA
genus[genus=="Hydrococus"] <- NA
genus[genus=="Actinastrum falcatus"] <- 'Ankistrodesmus falcatus' #ID error
genus[genus=="Cf. Actinastrum falcatus"] <- 'Ankistrodesmus falcatus' #ID error
genus[genus=="Ankistrodesmus falcatus"] <- NA #no biovolume info available 
genus[genus=="cryptomonad"] <- 'Cryptomonad'
genus[genus=="Cf. Cryptomonad"] <- 'Cryptomonad'
genus[genus=="Large Cryptomonad-esque cell husks"] <- 'Cryptomonad'
genus[genus=="Cryptomonas"] <- 'Cryptomonad'
genus[genus=="large Cryptomonad"] <- 'Cryptomonad'
genus[genus=="Dinobryon small"] <- 'Dinobryon'
genus[genus=="Cf. Cyanobium"] <- NA
genus[genus=="Actinastrum hantzchii"] <- NA
genus[genus=="Chamaecalyx"] <- NA
genus[genus=="Ankistrodesmus falcatus"] <- NA
genus[genus=="Actinastrum hantzchii"] <- NA
genus[genus=="Actinastrum hantzchii (colony)"] <- NA
genus[genus=="?10"] <- NA
genus[genus=="Hydrococcus"] <- NA
genus[genus=="Prestauroneis protracta"] <- NA
genus[genus=="Ulnaria cf. acus"] <- NA
genus[genus=="Closteriopsis longissima"] <- NA
genus[genus=="Karayevia"] <- NA
genus[genus=="Cymbella"] <- NA
genus[genus=="Eunota"] <- NA
genus[genus=="Staurastrum"] <- NA
genus[genus=="Chrysosphaerella"] <- NA
genus[genus=="Uroslenia"] <- NA
genus[genus=="Cf. Actinocyclus"] <- NA
genus[genus=="Cf. Fragilariforma constricta"] <- NA
genus[genus=="Cf. Chlorallantus oblongus"] <- NA
genus[genus=="Cf. Tetraedron victoriae"] <- NA
genus[genus=="Cf. Chlorallantus oblongus"] <- NA
genus[genus=="Eunotia"] <- NA
genus[genus=="Cf. Eutrepita globulifera"] <- NA
genus[genus=="Cf. Rossithidium linearis"] <- NA
genus[genus=="Ceratium"] <- NA
genus[genus=="Cf. Chlorallantis oblongus"] <- NA
genus[genus=="Cf. Anabaenopsis elenkinii (colony)"] <- NA
genus[genus=="Actinastrum falcatus"] <- NA
genus[genus=="unID chrysophyte"] <- NA
genus[genus=="Aphanothece bacilloidea"] <- NA
genus[genus=="Cyanobium"] <- NA
genus[genus=="Statospore"] <- NA
genus[genus=="unID pennate diatom with stauros"] <- NA
genus[genus=="Perdinium"] <- NA
genus[genus=="unID pennate diatom"] <- NA
genus[genus=="unID bacilliariales diatom"] <- NA
#genus[genus=="Cosmarium"] <- NA
genus[genus=="Actinastrum falcatus"] <- NA
genus[genus=="Cf. Ulnaria"] <- NA
genus[genus=="unID cilliate"] <- NA
genus[genus=="Fragilaria"] <- 'Fragilaria'
genus[genus=="Small green filament"] <- NA
genus[genus=="unID Ciliate"] <- NA
genus[genus=="Cf. Cymbella"] <- NA
genus[genus=="Planktothrix strand"] <- NA
genus[genus=="Cf. Aphanocapsa inserta colony"] <- NA
genus[genus=="Cf. Actinastrum falcatus"] <- NA
genus[genus=="Nitzschioid diatom"] <- 'Nitzschia'
genus[genus=="unID Naviculoid diatom"] <- NA
genus[genus=="Cf. Chlorallantus oblongus"] <- NA
genus[genus=="Pinnularia"] <- NA
genus[genus=="Cf. Raphidocelis subcapitata"] <- NA
genus[genus=="Cf. Decussata"] <- NA
genus[genus=="Fragilaria tenera"] <- NA
genus[genus=="Photosynthetic Euglenoid"] <- NA
genus[genus=="unID Ciliate"] <- NA
genus[genus=="Cf. Karayevia"] <- NA
genus[genus=="Cf. Staurosirella leptostauron var dubia"] <- NA
genus[genus=="Cf. Golenkinia"] <- NA
genus[genus=="Adlafia"] <- NA
genus[genus=="Cf. Golenkinia"] <- NA
genus[genus=="Fragillaria"] <- 'Fragilaria'
genus[genus=="Cyclostephanos invisitatus"] <- 'Cyclostephanos'
genus[genus=="Large Cryptomonad"] <- 'Cryptomonad'
genus[genus=="Large cryptomonad"] <- 'Cryptomonad'
genus[genus=="Cf. Sellaphora pupula"] <- 'Sellaphora'
genus[genus=="Cf. Encyonema"] <- 'Encyonema'
genus[genus=="Cf. Sellaphora (small)"] <- 'Sellaphora' #small are 110 bv not small are 120
genus[genus=="Cf. Elakatothrix"] <- 'Elakatothrix'
genus[genus=="Cf. Diatoma mesodon"] <- 'Diatoma mesodon'
genus[genus=="Cf. Planothidium"] <- 'Planothidium'
genus[genus=="Asterionella formosa"] <- 'Asterionella'
genus[genus=="Cf. Merispomedia"] <- 'Merismopedia'
genus[genus=="Cf. Hippodonta"] <- 'Hippodonta'
genus[genus=="Hippodonta capitata"] <- 'Hippodonta'
genus[genus=="Cyclotella meneghiniana"] <- 'Cyclotella'
genus[genus=="Cyclotella comensis"] <- 'Cyclotella'
genus[genus=="unID Cyclotella"] <- 'Cyclotella'
genus[genus=="Cf. Placoneis"] <- 'Placoneis'
genus[genus=="Cf. Asterocapsa"] <- 'Asterocapsa'
genus[genus=="Photosynthetic Euglena"] <- 'Euglena'
genus[genus=="Stephanodiscus cf. alpinus"] <- 'Stephanodiscus'
genus[genus=="Stephanodiscus hantzschii"] <- 'Stephanodiscus'
genus[genus=="Stephanodiscus niagarae"] <- 'Stephanodiscus'
genus[genus=="Stephanodiscus niagare"] <- 'Stephanodiscus'
genus[genus=="Stephanodiscus small"] <- 'Stephanodiscus'
genus[genus=="Cf. Pseudosaurosira"] <- 'Pseudostaurosira'
genus[genus=="Cf. Achnanthidium"] <- 'Achnanthidium'
genus[genus=="Staurosira construens"] <- 'Staurosira'
genus[genus=="Cf. Staurosira"] <- 'Staurosira'
genus[genus=="Gomphonema"] <- 'Gomphoneis'
genus[genus=="Synura colony"] <- 'Synura'
genus[genus=="Discostella stelligera"] <- 'Discostella'
genus[genus=="Cf. Anabaena"] <- 'Anabaena'
genus[genus=="Anabaena eucompacta"] <- 'Anabaena'
genus[genus=="Filamentous Green"] <- NA
genus[genus=="Cf. Aphanothece bacilloidea"] <- 'Aphanothece bacilloidea' #appears once
genus[genus=="Eunota"] <- NA 
genus[genus=="Cymbella"] <- NA
genus[genus=="Karayevia"] <- NA
genus[genus=="Coleosphaerum"] <- NA
genus[genus=="Hydrococcus"] <- NA
genus[genus=="Cf. Craspedostauros"] <- NA

#Cf. Tetraedron victoriae needs biovolume of 47.2472
#one Lindavia needs a bv of 44.42277385
#one peanut needs a bv 84.9851

#### filling winter col in with zeros
genus$avsnow[is.na(genus$avsnow)] = 0
genus$totice[is.na(genus$totice)] = 0
genus$whiteice[is.na(genus$whiteice)] = 0
genus$blueice[is.na(genus$blueice)] = 0
genus$perblueice[is.na(genus$perblueice)] = 0
genus$perwhiteice[is.na(genus$perwhiteice)] = 0

# add true 0s for all bvs for all genus - pivot-wide then zero, then pivot back to long
genus$sampledate = ymd(genus$sampledate)

genus.wide = genus

genus.wide %<>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = Genus, values_from = CellBioVol) %>%
  select(-row)

genus.long = pivot_longer(genus.wide, cols=12:70, names_to="Genus", values_to = "CellBioVol")
genus.long$CellBioVol[is.na(genus.long$CellBioVol)] = 0

ggplot(subset(genus.long, sampledate=='1997-01-14'), aes(Genus, CellBioVol))+
  geom_point()

write.csv(genus.long, 'data/genus_clean_03032020.csv', row.names = F)

#======== 

# start looking at genus-specific trends
genus = read.csv('data/genus_clean_03032020.csv', stringsAsFactors = F)
genus$CellBioVol[genus$CellBioVol==0] <- NA

genus$chlor = abs(genus$chlor)
genus$sampledate = ymd(genus$sampledate)

genus$ice.pres = ifelse(genus$totice > 0, 1, 0)

summary(genus)

unique(genus$Genus)

ggplot(genus, aes(as.factor(year), log(CellBioVol), fill=Genus))+
  geom_boxplot()

genus.ann = genus
genus.ann %<>% group_by(year, Genus) %>%
  summarize(med.bv = median(log(CellBioVol), na.rm=T))

genus.ann$med.bv[is.na(genus.ann$med.bv)] <- 0

ggplot(subset(genus,year<2000), aes(as.factor(year), log(CellBioVol), fill=Genus))+
  geom_boxplot(aes(group=Genus))+
  geom_line(data=subset(genus.ann, year<2000), aes(as.factor(year), med.bv, color=Genus, group=Genus))

ggplot(genus.ann, aes(as.factor(year), med.bv, color=Genus))+
  geom_point()+
  geom_line(aes(group=Genus))

# summarize species trends

tile=genus.ann
tile = arrange(tile,Genus)
tile$Genus = factor(tile$Genus, levels = rev(unique(tile$Genus)), ordered=TRUE)

#tile$med.h.bin = (1:7)[cut(tile$med.harv.kg.ha,breaks = c(-1,0.2,0.5,1,1.5,2,2.5,3))]

ggplot(tile, aes(x=year,y=Genus,fill=med.bv))+
  geom_tile(size=0.1,na.rm=T)+
  scale_fill_viridis_c(direction=-1)+
  theme_classic()
  
gen.keep = c("Armored Dinoflagellate","Naked Dinoflagellate","Limnothrix","Microcystis","Lindavia",
             "Cryptomonad", "Asterionella", "Dinobryon", "Fragilaria")


# load dataset of iceon and off dates
ice = read.csv('data/iceduration.csv', stringsAsFactors = F)
ice %<>% rename(ice.on = datefirstice,
                ice.off = datelastice)

ice$ice.on = mdy(ice$ice.on)
ice$ice.off = mdy(ice$ice.off)

ice %<>% subset(year>1996 & year <2010)
ice %<>% subset(lakeid == "SP")

# temporal trends in genus
ggplot(subset(genus, Genus %in% gen.keep), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_smooth(aes(color=Genus), se=F)+
  scale_color_brewer(palette = 'Paired')+
  theme_classic()

ggplot(subset(genus, Genus %in% gen.keep), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_smooth(aes(color=Genus), method='lm',se=F)+
  scale_color_brewer(palette = 'Paired')+
  theme_classic()

ice.labs <- c("Ice Off", "Ice On")
names(ice.labs) <- c(0, 1)

ggplot(subset(genus, Genus %in% gen.keep), aes(sampledate, log(CellBioVol), color=Genus))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point(data=subset(genus, Genus %in% gen.keep), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_smooth(data=subset(genus, Genus %in% gen.keep), aes(sampledate, log(CellBioVol), color=Genus), method='lm', se=F)+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))+
  theme_classic()+
  labs(x='Year')


genus2[Genus==is.na] <- 'Other'

gen.nine = c('NA',"Asterocapsa","Cocconeis","Flagellated Green", "Cf. Komvophoron / Trichormus","Cyanobacteria","Elakatothrix","Mallomonas",
                         "Segmented Green", "Peanut")

gen.8 = c("Asterocapsa","Cocconeis","Flagellated Green", "Cf. Komvophoron / Trichormus","Cyanobacteria","Elakatothrix","Mallomonas",
             "Segmented Green", "Peanut")

ggplot(subset(genus, Genus %in% gen.nine), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_smooth(aes(color=Genus), se=F)+
  scale_color_brewer(palette = 'Paired')+
  theme_classic()

ggplot(subset(genus, Genus %in% gen.nine), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_smooth(aes(color=Genus), method='lm',se=F)+
  scale_color_brewer(palette = 'Paired')+
  theme_classic()

ice.labs <- c("Ice Off", "Ice On")
names(ice.labs) <- c(0, 1)

ggplot(subset(genus, Genus %in% gen.nine), aes(sampledate, log(CellBioVol), color=Genus))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point(data=subset(genus, Genus %in% gen.nine), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_smooth(data=subset(genus, Genus %in% gen.nine), aes(sampledate, log(CellBioVol), color=Genus), method='lm', se=F)+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))+
  theme_classic()+
  labs(x='Year')


#capture zeros in this set 

genus2= read.csv('data/clean_abiotic_genus_03262020.csv', stringsAsFactors = F)
genus2$sampledate = mdy(genus2$sampledate)

genus3= read.csv('data/Na_as_Other_clean_abiotic_genus_03262020.csv', stringsAsFactors = F)
genus3$sampledate = mdy(genus3$sampledate)

genus[genus=="Cf. Craspedostauros"] <- NA


ggplot(subset(genus2, Genus %in% gen.keep), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_smooth(aes(color=Genus), se=F)+
  scale_color_brewer(palette = 'Paired')+
  theme_classic()

ggplot(subset(genus2, Genus %in% gen.keep), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_smooth(aes(color=Genus), method='lm',se=F)+
  scale_color_brewer(palette = 'Paired')+
  theme_classic()

ice.labs <- c("Ice Off", "Ice On")
names(ice.labs) <- c(0, 1)

ggplot(subset(genus2, Genus %in% gen.keep), aes(sampledate, log(CellBioVol), color=Genus))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point(data=subset(genus, Genus %in% gen.keep), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_smooth(data=subset(genus, Genus %in% gen.keep), aes(sampledate, log(CellBioVol), color=Genus), method='lm', se=T)+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))+
  theme_classic()+
  labs(x='Year')

ggplot(subset(genus2, Genus %in% gen.8), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_smooth(aes(color=Genus), se=T)+
  scale_color_brewer(palette = 'Paired')+
  theme_classic()

ggplot(subset(genus2, Genus %in% gen.8), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_smooth(aes(color=Genus), method='lm',se=F)+
  scale_color_brewer(palette = 'Paired')+
  theme_classic()

ice.labs <- c("Ice Off", "Ice On")
names(ice.labs) <- c(0, 1)

ggplot(subset(genus2, Genus %in% gen.8), aes(sampledate, log(CellBioVol), color=Genus))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point(data=subset(genus2, Genus %in% gen.8), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_smooth(data=subset(genus2, Genus %in% gen.8), aes(sampledate, log(CellBioVol), color=Genus), method='lm', se=F)+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))+
  theme_classic()+
  labs(x='Year')

ggplot(genus2$Genus, aes(sampledate, log(CellBioVol), color=Genus))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_smooth(aes(color=Genus), se=F)+
  scale_color_brewer(palette = 'Paired')+
  theme_classic()

ggplot(subset(genus2, Genus %in% gen.keep), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_line(linetype='dotted')+
  geom_smooth(aes(color=Genus), method='lm',se=F)+
  scale_color_brewer(palette = 'Paired')+
  theme_classic()

gen.dinos<- c('Naked Dinoflagellate','Armored Dinoflagellate')
gen.main = c("Armored Dinoflagellate","Naked Dinoflagellate","Limnothrix","Microcystis","Lindavia",
             "Cryptomonad", "Asterionella", "Dinobryon", "Fragilaria", "Asterocapsa","Cocconeis",
             "Flagellated Green", "Cf. Komvophoron / Trichormus","Cyanobacteria","Elakatothrix",
             "Mallomonas", "Segmented Green", "Peanut")

gen.main.total = c("Armored Dinoflagellate","Naked Dinoflagellate","Limnothrix","Microcystis","Lindavia",
                   "Cryptomonad", "Asterionella", "Dinobryon", "Fragilaria","Cocconeis",
                   "Flagellated Green", "Cf. Komvophoron / Trichormus","Cyanobacteria","Elakatothrix","Mallomonas",
                   "Segmented Green", "Peanut","Other")


#Asterocapsa-- NOT ENOUGH DATA

gen.ndmi=c("Armored Dinoflagellate","Cyanobacteria")

ggplot(subset(genus2, Genus %in%gen.ndmi), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point(size=3)+
  geom_line(linetype='dotted')+
  geom_smooth(aes(color=Genus), method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')+
  theme_classic()


ice.labs <- c("Ice Off", "Ice On")
names(ice.labs) <- c(0, 1)

library(RColorBrewer)
colourCount = length(unique(gen.main.total))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

ggplot(subset(genus2, Genus %in% gen.ndmi), aes(sampledate, log(CellBioVol), color=Genus))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point(data=subset(genus2, Genus %in% gen.ndmi), aes(sampledate, log(CellBioVol), color=Genus), size=3)+
  geom_smooth(data=subset(genus2, Genus %in% gen.ndmi), aes(sampledate, log(CellBioVol), color=Genus), method='lm', se=T)+
  #scale_fill_brewer(palette = 'Set1')+
  geom_line(linetype='dotted')+
  scale_fill_manual(values = getPalette(colourCount))+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))+
  theme_classic()+
  labs(x='Year')

gen.main = c("Armored Dinoflagellate","Naked Dinoflagellate","Limnothrix","Microcystis","Lindavia",
             "Cryptomonad", "Asterionella", "Dinobryon", "Fragilaria", "Asterocapsa","Cocconeis",
             "Flagellated Green", "Cf. Komvophoron / Trichormus","Cyanobacteria","Elakatothrix",
             "Mallomonas", "Segmented Green", "Peanut")

gen.main.total = c("Armored Dinoflagellate","Naked Dinoflagellate","Limnothrix","Microcystis","Lindavia",
             "Cryptomonad", "Asterionella", "Dinobryon", "Fragilaria", "Asterocapsa","Cocconeis",
             "Flagellated Green", "Cf. Komvophoron / Trichormus","Cyanobacteria","Elakatothrix","Mallomonas",
             "Segmented Green", "Peanut","Total")



ggplot(subset(genus2, Genus %in% gen.main.total), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_line(linetype='dotted')+
 # geom_smooth(aes(color=Genus), se=F)+
  scale_color_brewer(palette = 'Paired')+
  theme_classic()

ggplot(subset(genus2, Genus %in% gen.8), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_smooth(aes(color=Genus), method='lm',se=F)+
  scale_color_brewer(palette = 'Paired')+
  theme_classic()


## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz 
library(lubridate) # dealing with dates

#===========
# NO NEED TO RERUN - SKIP AHEAD 
# deal with abiotic data

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
algae = read.csv('data/biovolume1.csv',stringsAsFactors = F)

algae %<>% select(sampledate, Genus, CellBioVol)

algae$sampledate = mdy(algae$sampledate)
algae$daynum = yday(algae$sampledate)
algae$year = year(algae$sampledate)

data = full_join(abiotic, algae, by=c("year","daynum","sampledate"))

data %<>% subset(year > 1996)

write.csv(data, 'data/clean_algae_abiotic_02252020.csv',row.names = F)


#======================
data = read.csv('data/clean_algae_abiotic_02252020.csv',stringsAsFactors = F)

# pull out total biovolumes
totals = subset(data, Genus == "TotalBiovolume")
totals$chlor = abs(totals$chlor)

# pull out genus-specific biovolumes
genus = subset(data, Genus != "TotalBiovolume")

# paired-plot of predictors and total biovolumes for co-linearity
pairs(totals[,c(4:11,13)])

# assess distrubition of variables
hist(totals$chlor)
hist(log(totals$chlor)) # log chlor

hist(totals$avsnow) # no transform

hist(totals$totice)
hist(log(totals$totice)) # bit better log

hist(totals$whiteice) # no transform

hist(totals$blueice) 
hist(log(totals$blueice)) # bit better log, maybe unneccessary?

hist(totals$light)
hist(log(totals$light)) # bit better log

hist(totals$CellBioVol)
hist(log(totals$CellBioVol)) # log bv

#=========
totals %<>% select(-Genus)
totals$log.chlor = log(totals$chlor)
totals$log.bv = log(totals$CellBioVol)
totals.long = pivot_longer(totals, cols=c("chlor","log.chlor","avsnow","totice","whiteice","blueice","perwhiteice","perblueice",
                                          "light","CellBioVol","log.bv"), names_to="variable", values_to = "value")


ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')

ggplot(totals.long, aes(year, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')

totals.long$month = month(totals.long$sampledate)

winter = subset(totals.long, month < 4)

#only pull out winter sampling
ggplot(winter, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')

ggplot(winter, aes(year, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')

ggplot(winter, aes(year, value, color=variable))+
  geom_point()+
  geom_smooth(method='lm', aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')


#==============
# check temporal bv trend 
totals$month = month(totals$sampledate)
winter.totals = subset(totals, month <4)

winter.totals$ctr.year = winter.totals$year-mean(winter.totals$year)

# temporal only, no random effects
bv.1 = lm(log.bv ~ ctr.year, data=winter.totals)
AIC(bv.1) 
summary(bv.1) # not signif

# random intercept - temporal only
bv.2 = lme(log.bv ~ ctr.year, random = ~1|ctr.year, method ='REML', data=winter.totals)
summary(bv.2) # not sig


model = subset(winter.totals,!is.na(chlor))
model.2 = subset(model, !is.na(light))


# 2. begin incorporating random effects 's(year,bs="re")' 
bv.a = lme(log.bv ~ log.chlor + avsnow + totice + blueice + light, random = ~1|ctr.year, data=model.2)
summary(bv.a) # not significant

# get rid of chloro - least sig, also proxy(ish) for bv
bv.b = lme(log.bv ~ avsnow + totice + blueice + light, random = ~1|ctr.year, data=model.2)
summary(bv.b) # not significant

# get rid of light - least sig
bv.c = lme(log.bv ~ avsnow + totice + blueice, random = ~1|ctr.year, data=model.2)
summary(bv.c) # blue ice not significant

# get rid of blue ice - least sig
bv.d = lme(log.bv ~ avsnow + totice, random = ~1|ctr.year, data=model.2)
summary(bv.d) # blue ice not significant

# get rid of total ice - least sig
bv.e = lme(log.bv ~ avsnow + blueice, random = ~1|ctr.year, data=model.2)
summary(bv.e) # blue ice not significant

# get rid of total ice - least sig
bv.f = lme(log.bv ~ avsnow, random = ~1|ctr.year, data=model.2)
summary(bv.f) # significant

#========
# genus trends over time

d<- unique(genus$Genus)# shows all names there
d
write.csv(d, 'data/unique.genus.csv')

#==============
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

#======== 
genus$sampledate =  ymd(genus$sampledate)

start = c("Limnothrix", "Microcystis", "Peanut", "Naked Dinoflagellate", "Dinobryon", "Armored Dinoflagellate")

ggplot(subset(genus,Genus %in% start), aes(year, log(CellBioVol), color=Genus))+
  geom_point(aes(group=Genus))+
  geom_smooth(aes(group=Genus),se=F)

ggplot(subset(genus,Genus %in% start), aes(daynum, log(CellBioVol), color=Genus))+
  geom_point(aes(group=Genus))+
  geom_smooth(aes(group=Genus),se=F)


ggplot(genus, aes(year, log(CellBioVol), color=Genus))+
  geom_point(aes(group=Genus))+
  geom_smooth(aes(group=Genus),se=F)

e<-ggplot(genus, aes(sampledate, log(CellBioVol), color=Genus))+
  geom_point(aes(group=Genus))+
  geom_smooth(aes(group=Genus),se=F)
e

start = c("Limnothrix", "Microcystis", "Peanut","Lindavia","Cyclostephanos","Cryptomonad","Cf. Komvophoron / Trichormus", "Naked Dinoflagellate", "Cf. Planktolyngabia","Cocconeis")
a<- ggplot(subset(genus,Genus %in% start), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_point(aes(group=Genus))+
  geom_smooth(aes(group=Genus),se=F)
a

middle1 = c("Rhoicosphenia", "Armored Dinoflagellate", "Sellaphora","Diatoma mesodon","Navicula","Dinobryon","Elakatothrix", "Nostoc", "Cf. Craspedostauros","Cyanobacteria")
b<- ggplot(subset(genus,Genus %in% middle1), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_point(aes(group=Genus))+
  geom_smooth(aes(group=Genus),se=F)
b

middle2 = c("Fragilaria", "Asterionella", "Merismopedia","Cf. Cavinula","Diploneis","Encyonema","Planothidium", "Hippodonta", "Cf. Coelosphaerium","Tetraedron victoriae")
c<- ggplot(subset(genus,Genus %in% middle2), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_point(aes(group=Genus))+
  geom_smooth(aes(group=Genus),se=F)
c


middle3 = c("Placoneis", "Epithemia", "Asterocapsa","Euglena","Flagellated Green","Tabellaria","Aulacoseira", "Hippodonta", "Stephanodiscus","Pseudostaurosira")
f<- ggplot(subset(genus,Genus %in% middle3), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_point(aes(group=Genus))+
  geom_smooth(aes(group=Genus),se=F)
f

middle4 = c("Stauroforma", "Achnanthidium", "Segmented Green","Gomphoneis","Mallomonas","Synura","Nitzschia", "Cyclotella", "Discostella","Anabaena")
g<- ggplot(subset(genus,Genus %in% middle4), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_point(aes(group=Genus))+
  geom_smooth(aes(group=Genus),se=F)
g


end = c("Chroococcus", "Staurosira", "Planktothrix","Small Chroococcus","Long thin rod","Aphanothece bacilloidea","Gloeocapsa", NA)
h<- ggplot(subset(genus,Genus %in% end), aes(sampledate, log(CellBioVol), color=Genus))+
  geom_point(aes(group=Genus))+
  geom_smooth(aes(group=Genus),se=F)
h


#========== 
#### filling winter col in with zeros
genus$avsnow[is.na(genus$avsnow)] = 0
genus$totice[is.na(genus$totice)] = 0
genus$whiteice[is.na(genus$whiteice)] = 0
genus$blueice[is.na(genus$blueice)] = 0
genus$perblueice[is.na(genus$perblueice)] = 0
genus$perwhiteice[is.na(genus$perwhiteice)] = 0

#==============
#filling in bvs with 0s when needed, pair sampledate.df with genus.df

#want year|daynumn|sampledate|genus|bv|variables

# Create datafame of all possible date and Genus combinations
all.genus = unique(genus$Genus[!is.na(genus$Genus)]) #without NA
all.dates = unique(genus$sampledate)
genus.combo = expand.grid(sampledate = all.dates,Genus = all.genus) %>% arrange(sampledate)

join.genus = genus.combo %<>% left_join(genus, by = c('sampledate', 'Genus'))

#HD: however, you probably want to just join biovolume and add the physical parameters after
join.genus = genus.combo %>% left_join(select(genus,sampledate,Genus,CellBioVol), by = c('sampledate', 'Genus')) %>% 
  mutate(CellBioVol = replace_na(CellBioVol, 0)) %>% # reeplace na with zeros
  mutate(year = year(sampledate)) %>% # add back year column 
  left_join(abiotic, by = c('year','sampledate'))

#===============
ggplot(subset(genus,Genus %in% start), aes(year, avsnow, color=Genus))+
  geom_point(aes(group=Genus))+
  geom_smooth(aes(group=Genus),se=F)

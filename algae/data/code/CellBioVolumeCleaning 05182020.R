## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz 
library(lubridate) # dealing with dates
library(ggpubr)


# deal with algae dataset
algae = read.csv('data/biovolumereal.csv',stringsAsFactors = F)

algae %<>% select(sampledate, Genus, CellBioVol)

algae$sampledate = mdy(algae$sampledate)
algae$daynum = yday(algae$sampledate)
algae$year = year(algae$sampledate)

#data = full_join(abiotic, algae, by=c("year","daynum","sampledate"))
#data %<>% subset(year > 1996)
#write.csv(data, 'data/clean_algae_abiotic_03032020.csv',row.names = F)

#data = read.csv('data/clean_algae_abiotic_03032020.csv',stringsAsFactors = F)

# pull out total biovolumes
totals = subset(algae, Genus == "TotalBiovolume")

# pull out genus-specific biovolumes
# skip ahead to clean file
genus = subset(algae, Genus != "TotalBiovolume")

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

unique(genus$Genus)

class(genus$Genus)

genus$Genus[genus$Genus=="unID cyanobacteria (colony)"] <- NA
genus$Genus[genus$Genus=="?10"] <- NA
genus$Genus[genus$Genus=="Cyclotella"] <- 'Lindavia comensis'
genus$Genus[genus$Genus=="Cyclotella ocellata"] <- 'Lindavia comensis'
genus$Genus[genus$Genus=="Cyclotella distinguenda"] <- 'Lindavia comensis'
genus$Genus[genus$Genus=="?11"] <- NA
genus$Genus[genus$Genus=="Microplastic - blue filament"] <- NA
genus$Genus[genus$Genus=="Microcyctis"] <- 'Microcystis'
genus$Genus[genus$Genus=="Microcystis (small)"] <- 'Microcystis'
genus$Genus[genus$Genus=="Microcystis (large)"] <- 'Microcystis'
genus$Genus[genus$Genus=="Thin Rod"] <- NA
genus$Genus[genus$Genus=="Lindavia comensis"] <- 'Lindavia'
genus$Genus[genus$Genus=="Lindavia affinis"] <- 'Lindavia'
genus$Genus[genus$Genus=="Lindavia "] <- 'Lindavia'
genus$Genus[genus$Genus=="Lindavia (Lindavia affinis)"] <- 'Lindavia'
genus$Genus[genus$Genus=="Lindavia cf. bodanica"] <- 'Lindavia'
#genus[Genus=="Lindavia"]== genus[BioVolume== '44.42277385']
genus$Genus[genus$Genus=="Cf. Cryptomonad (NEW CATEGORY)"] <- 'Cf. Cryptomonad'
genus$Genus[genus$Genus=="Segmented green"] <- 'Segmented Green'
genus$Genus[genus$Genus=="Mallomonas c"] <- 'Mallomonas'
genus$Genus[genus$Genus=="Mallomonas (colony)"] <- 'Mallomonas'
genus$Genus[genus$Genus=="Mallomonas Colony"] <- 'Mallomonas'
genus$Genus[genus$Genus=="Lindavia comensis (C. Comensis))"] <- 'Lindavia'
genus$Genus[genus$Genus=="Flagellated green"] <- 'Flagellated Green'
genus$Genus[genus$Genus=="Cyanobacterium diachloros"] <- 'Cyanobacteria'
genus$Genus[genus$Genus=="Cyanobacteria diachloros"] <- 'Cyanobacteria'
genus$Genus[genus$Genus=="Cyanobacteria diachlorus"] <- 'Cyanobacteria'
genus$Genus[genus$Genus=="unID colonial Cyanobacteria"] <- 'Cyanobacteria'
genus$Genus[genus$Genus=="unID Cyanobacteria"] <- 'Cyanobacteria'
genus$Genus[genus$Genus=="unID cyanobacteria"] <- 'Cyanobacteria'
genus$Genus[genus$Genus=="Thin rod"] <- NA
genus$Genus[genus$Genus=="Lindavia comensis (C. Comensis)"] <- 'Lindavia'
genus$Genus[genus$Genus=="Aulacoseira cf. subarctica"] <- 'Aulacoseira'
genus$Genus[genus$Genus=="Aulacoseira cf. ambigua"] <- 'Aulacoseira'
genus$Genus[genus$Genus=="Aulacoseira cf. islandica"] <- 'Aulacoseira'
genus$Genus[genus$Genus=="Chroococcus cf. kidneys"] <- 'Chroococcus'
genus$Genus[genus$Genus=="Chroococcus "] <- 'Chroococcus'
genus$Genus[genus$Genus=="Cosmarium (Croococcus cf. kidneys)"] <- 'Chroococcus'
genus$Genus[genus$Genus=="Croococcus cf. kidneys"] <- 'Chroococcus'
genus$Genus[genus$Genus=="Small chroococcus"] <- 'Small Chroococcus'
genus$Genus[genus$Genus=="Dinobryon internal organelles ONLY"] <- NA
genus$Genus[genus$Genus=="Cf. Fragilariforma constricta"] <- NA
genus$Genus[genus$Genus=="?17"] <- 'Chroococcus cf. kidneys'
genus$Genus[genus$Genus=="Fragilaria crototensis"] <- 'Fragilaria'
genus$Genus[genus$Genus=="Fragilaria "] <- 'Fragilaria'
genus$Genus[genus$Genus=="Fragilaria intermedia"] <- 'Fragilaria'
genus$Genus[genus$Genus=="Fragilaria tenera"] <- 'Fragilaria'
genus$Genus[genus$Genus=="Fragilaria crotonensis"] <- 'Fragilaria'
genus$Genus[genus$Genus=="Planktothrix cells"] <- 'Planktothrix'
genus$Genus[genus$Genus=="Planktothrix strand"] <- NA
genus$Genus[genus$Genus=="cf. Planktolyngabia"] <- 'Planktolyngabia'
genus$Genus[genus$Genus=="NA"] <- NA
genus$Genus[genus$Genus=="Mallomonas colony"] <- 'Mallomonas'
genus$Genus[genus$Genus=="Discostella "] <- 'Discostella' #one bv is zero need to change to 350.7702735
genus$Genus[genus$Genus=="Discotella"] <- 'Discostella'
genus$Genus[genus$Genus=="Small Cocconeis"] <- 'Cocconeis'
genus$Genus[genus$Genus=="Small cocconeis"] <- 'Cocconeis'
genus$Genus[genus$Genus=="unID Cocconeis"] <- 'Cocconeis'
genus$Genus[genus$Genus=="Small cocconies"] <- 'Cocconeis'
genus$Genus[genus$Genus=="Hydrococcus"] <- NA
genus$Genus[genus$Genus=="Hydrococus"] <- NA
genus$Genus[genus$Genus=="Actinastrum falcatus"] <- 'Ankistrodesmus falcatus' #ID error
genus$Genus[genus$Genus=="Cf. Actinastrum falcatus"] <- 'Ankistrodesmus falcatus' #ID error
genus$Genus[genus$Genus=="Ankistrodesmus falcatus"] <- NA #no biovolume info available 
genus$Genus[genus$Genus=="cryptomonad"] <- 'Cryptomonad'
genus$Genus[genus$Genus=="Cf. Cryptomonad"] <- 'Cryptomonad'
genus$Genus[genus$Genus=="Large Cryptomonad-esque cell husks"] <- 'Cryptomonad'
genus$Genus[genus$Genus=="Cryptomonas"] <- 'Cryptomonad'
genus$Genus[genus$Genus=="large Cryptomonad"] <- 'Cryptomonad'
genus$Genus[genus$Genus=="Dinobryon small"] <- 'Dinobryon'
genus$Genus[genus$Genus=="Cf. Cyanobium"] <- NA
genus$Genus[genus$Genus=="Actinastrum hantzchii"] <- NA
genus$Genus[genus$Genus=="Chamaecalyx"] <- NA
genus$Genus[genus$Genus=="Ankistrodesmus falcatus"] <- NA
genus$Genus[genus$Genus=="Actinastrum hantzchii"] <- NA
genus$Genus[genus$Genus=="Actinastrum hantzchii (colony)"] <- NA
genus$Genus[genus$Genus=="?10"] <- NA
genus$Genus[genus$Genus=="Hydrococcus"] <- NA
genus$Genus[genus$Genus=="Prestauroneis protracta"] <- NA
genus$Genus[genus$Genus=="Ulnaria cf. acus"] <- NA
genus$Genus[genus$Genus=="Closteriopsis longissima"] <- NA
genus$Genus[genus$Genus=="Karayevia"] <- NA
genus$Genus[genus$Genus=="Cymbella"] <- NA
genus$Genus[genus$Genus=="Eunota"] <- NA
genus$Genus[genus$Genus=="Staurastrum"] <- NA
genus$Genus[genus$Genus=="Chrysosphaerella"] <- NA
genus$Genus[genus$Genus=="Uroslenia"] <- NA
genus$Genus[genus$Genus=="Cf. Actinocyclus"] <- NA
genus$Genus[genus$Genus=="Cf. Fragilariforma constricta"] <- NA
genus$Genus[genus$Genus=="Cf. Chlorallantus oblongus"] <- NA
genus$Genus[genus$Genus=="Cf. Tetraedron victoriae"] <- NA
genus$Genus[genus$Genus=="Cf. Chlorallantus oblongus"] <- NA
genus$Genus[genus$Genus=="Eunotia"] <- NA
genus$Genus[genus$Genus=="Cf. Eutrepita globulifera"] <- NA
genus$Genus[genus$Genus=="Cf. Rossithidium linearis"] <- NA
genus$Genus[genus$Genus=="Ceratium"] <- NA
genus$Genus[genus$Genus=="Cf. Chlorallantis oblongus"] <- NA
genus$Genus[genus$Genus=="Cf. Anabaenopsis elenkinii (colony)"] <- NA
genus$Genus[genus$Genus=="Actinastrum falcatus"] <- NA
genus$Genus[genus$Genus=="unID chrysophyte"] <- NA
genus$Genus[genus$Genus=="Aphanothece bacilloidea"] <- NA
genus$Genus[genus$Genus=="Cyanobium"] <- NA
genus$Genus[genus$Genus=="Statospore"] <- NA
genus$Genus[genus$Genus=="unID pennate diatom with stauros"] <- NA
genus$Genus[genus$Genus=="Perdinium"] <- NA
genus$Genus[genus$Genus=="unID pennate diatom"] <- NA
genus$Genus[genus$Genus=="unID bacilliariales diatom"] <- NA
#genus[genus=="Cosmarium"] <- NA
genus$Genus[genus$Genus=="Actinastrum falcatus"] <- NA
genus$Genus[genus$Genus=="Cf. Ulnaria"] <- NA
genus$Genus[genus$Genus=="unID cilliate"] <- NA
genus$Genus[genus$Genus=="Fragilaria"] <- 'Fragilaria'
genus$Genus[genus$Genus=="Small green filament"] <- NA
genus$Genus[genus$Genus=="unID Ciliate"] <- NA
genus$Genus[genus$Genus=="Cf. Cymbella"] <- NA
genus$Genus[genus$Genus=="Planktothrix strand"] <- NA
genus$Genus[genus$Genus=="Cf. Aphanocapsa inserta colony"] <- NA
genus$Genus[genus$Genus=="Cf. Actinastrum falcatus"] <- NA
genus$Genus[genus$Genus=="Nitzschioid diatom"] <- 'Nitzschia'
genus$Genus[genus$Genus=="unID Naviculoid diatom"] <- NA
genus$Genus[genus$Genus=="Cf. Chlorallantus oblongus"] <- NA
genus$Genus[genus$Genus=="Pinnularia"] <- NA
genus$Genus[genus$Genus=="Cf. Raphidocelis subcapitata"] <- NA
genus$Genus[genus$Genus=="Cf. Decussata"] <- NA
genus$Genus[genus$Genus=="Fragilaria tenera"] <- NA
genus$Genus[genus$Genus=="Photosynthetic Euglenoid"] <- NA
genus$Genus[genus$Genus=="unID Ciliate"] <- NA
genus$Genus[genus$Genus=="Cf. Karayevia"] <- NA
genus$Genus[genus$Genus=="Cf. Staurosirella leptostauron var dubia"] <- NA
genus$Genus[genus$Genus=="Cf. Golenkinia"] <- NA
genus$Genus[genus$Genus=="Adlafia"] <- NA
genus$Genus[genus$Genus=="Cf. Golenkinia"] <- NA
genus$Genus[genus$Genus=="Fragillaria"] <- 'Fragilaria'
genus$Genus[genus$Genus=="Cyclostephanos invisitatus"] <- 'Cyclostephanos'
genus$Genus[genus$Genus=="Large Cryptomonad"] <- 'Cryptomonad'
genus$Genus[genus$Genus=="Large cryptomonad"] <- 'Cryptomonad'
genus$Genus[genus$Genus=="Cf. Sellaphora pupula"] <- 'Sellaphora'
genus$Genus[genus$Genus=="Cf. Encyonema"] <- 'Encyonema'
genus$Genus[genus$Genus=="Cf. Sellaphora (small)"] <- 'Sellaphora' #small are 110 bv not small are 120
genus$Genus[genus$Genus=="Cf. Elakatothrix"] <- 'Elakatothrix'
genus$Genus[genus$Genus=="Cf. Diatoma mesodon"] <- 'Diatoma mesodon'
genus$Genus[genus$Genus=="Cf. Planothidium"] <- 'Planothidium'
genus$Genus[genus$Genus=="Asterionella formosa"] <- 'Asterionella'
genus$Genus[genus$Genus=="Cf. Merispomedia"] <- 'Merismopedia'
genus$Genus[genus$Genus=="Cf. Hippodonta"] <- 'Hippodonta'
genus$Genus[genus$Genus=="Hippodonta capitata"] <- 'Hippodonta'
genus$Genus[genus$Genus=="Cyclotella meneghiniana"] <- 'Cyclotella'
genus$Genus[genus$Genus=="Cyclotella comensis"] <- 'Cyclotella'
genus$Genus[genus$Genus=="unID Cyclotella"] <- 'Cyclotella'
genus$Genus[genus$Genus=="Cf. Placoneis"] <- 'Placoneis'
genus$Genus[genus$Genus=="Cf. Asterocapsa"] <- 'Asterocapsa'
genus$Genus[genus$Genus=="Photosynthetic Euglena"] <- 'Euglena'
genus$Genus[genus$Genus=="Stephanodiscus cf. alpinus"] <- 'Stephanodiscus'
genus$Genus[genus$Genus=="Stephanodiscus hantzschii"] <- 'Stephanodiscus'
genus$Genus[genus$Genus=="Stephanodiscus niagarae"] <- 'Stephanodiscus'
genus$Genus[genus$Genus=="Stephanodiscus niagare"] <- 'Stephanodiscus'
genus$Genus[genus$Genus=="Stephanodiscus small"] <- 'Stephanodiscus'
genus$Genus[genus$Genus=="Cf. Pseudosaurosira"] <- 'Pseudostaurosira'
genus$Genus[genus$Genus=="Cf. Achnanthidium"] <- 'Achnanthidium'
genus$Genus[genus$Genus=="Staurosira construens"] <- 'Staurosira'
genus$Genus[genus$Genus=="Cf. Staurosira"] <- 'Staurosira'
genus$Genus[genus$Genus=="Gomphonema"] <- 'Gomphoneis'
genus$Genus[genus$Genus=="Synura colony"] <- 'Synura'
genus$Genus[genus$Genus=="Discostella stelligera"] <- 'Discostella'
genus$Genus[genus$Genus=="Cf. Anabaena"] <- 'Anabaena'
genus$Genus[genus$Genus=="Anabaena eucompacta"] <- 'Anabaena'
genus$Genus[genus$Genus=="Filamentous Green"] <- NA
genus$Genus[genus$Genus=="Cf. Aphanothece bacilloidea"] <- 'Aphanothece bacilloidea' #appears once
genus$Genus[genus$Genus=="Eunota"] <- NA 
genus$Genus[genus$Genus=="Cymbella"] <- NA
genus$Genus[genus$Genus=="Karayevia"] <- NA
genus$Genus[genus$Genus=="Coleosphaerum"] <- NA
genus$Genus[genus$Genus=="Hydrococcus"] <- NA
genus$Genus[genus$Genus=="Cf. Craspedostauros"] <- NA


genus2 = genus 

#Cf. Tetraedron victoriae needs biovolume of 47.2472
#one Lindavia needs a bv of 44.42277385
#one peanut needs a bv 84.9851

write.csv(genus2, 'data/cleanedCellBioVol05182020')

# add true 0s for all bvs for all genus - pivot-wide then zero, then pivot back to long
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

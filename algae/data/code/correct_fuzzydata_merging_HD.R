## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin)


######====
#Hilary's example
chlorophyll = data.frame(chloroDate = c(as.Date('2000-01-30'), as.Date('2003-01-30'),
                                        as.Date('2004-01-30'), as.Date('2006-01-30')),
                         num = c(23,12,16,18))
# Create columns of date range. In this case, date + 1, and date - 1
phytos = data.frame(phytoDate = c(as.Date('2000-01-30'), as.Date('2003-01-29'), as.Date('2006-01-30')),
                    num2 = c(20:22)) %>%
  mutate(datePlus1 = phytoDate + 1) %>% mutate(dateMinus1 = phytoDate - 1)
# Use a fuzzy join to compare the two columns to the chlorophyll date
fuzzy_left_join(chlorophyll, phytos, by = c("chloroDate" = "datePlus1", "chloroDate" = "dateMinus1"),
                match_fun = list(`<=`, `>=`))

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

totalbv = read.csv("data/TotalBVs.csv", stringsAsFactors = F)
totalbv= read.csv("../TotalBVs.csv")
totalbv$sampledate = ymd(totalbv$sampledate)
totalbv %<>% select(sampledate, Genus, CellBioVol) %>%
  mutate(bv.datePlus1 = sampledate + 1) %>% mutate(bv.dateMinus1 = sampledate - 1)

join_surfchlor <-fuzzy_left_join(join_ice, totalbv, by = c("sampledate" = "bv.datePlus1", "sampledate" = "bv.dateMinus1"),
                       match_fun = list(`<=`, `>=`))

join_surfchlor %<>% rename(CBV.date = sampledate.y)

ahhhhhhhh = full_join(join_surfchlor, Genera, by=c('CBV.date'))
abiotic %<>% rename(year = year4)

#join genera to this~

Genera= read.csv("data/biovolumereal.csv", stringsAsFactors = F)
Genera$sampledate = mdy(Genera$sampledate)
Genera %<>% select(sampledate, Genus, CellBioVol, PerBioVol)
Genera %<>% rename(CBV.date = sampledate)


Genera$Genus[Genus=="unID cyanobacteria (colony)"] <- NA
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




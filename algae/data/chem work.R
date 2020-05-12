## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz 
library(lubridate) # dealing with dates

#####============================

totals = read.csv("data/clean_abiotic_genus_03262020.csv", stringsAsFactors = F)
totals$sampledate = mdy(totals$sampledate)
totals 
totals$frlight[totals$frlight=="1"] <- NA #one iceon point with no light point to calc frlight against 


lightext = read.csv("data/light_ext_v1-2.csv", stringsAsFactors = F)
lightext$sampledate = mdy(lightext$sampledate)


lightextfull = full_join(totals, lightext, by=c("sampledate"))


#define seasons
ice = read.csv('data/iceduration.csv', stringsAsFactors = F)
ice %<>% rename(ice.on = datefirstice,
                ice.off = datelastice)

ice$ice.on = mdy(ice$datefirstice)
ice$ice.off = mdy(ice$datelastice)

ice %<>% subset(year>1996 & year <2010)
ice %<>% subset(lakeid == "SP")

ice.labs <- c("Ice Off", "Ice On")
names(ice.labs) <- c(0, 1)




	
"tpm","cl",	"so4",	"ca",	"mg",	"na",	"k",	"fe", "mn",	"chlor.surf",	"avsnow",	"totice",
"whiteice", "blueice","light","iceduration"), names_to="variable", values_to = "value"))


#value by year 
ggplot(totals, aes(sampledate, chlor.int))+
  geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point()+
  geom_smooth()+
  theme_classic()

#in the ~seasons~

ggplot(totals, aes(chlor.int, log(CellBioVol)))+
  #geom_vline(data=ice, aes(xintercept=ice.on), linetype='dashed')+
  #geom_vline(data=ice, aes(xintercept=ice.off), linetype='dotted')+
  geom_point(chlor.int)+
  scale_color_viridis_c(option="viridis")+
  geom_smooth()+
  #scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))+
  theme_classic()+
  labs(x='Year')

summary(summer$chlor.surf)


hist(totals$chlor.surf)
hist(log(totals$chlor.surf))
shapiro.test(log(totals$chlor.surf))

#####==========
#Chlor surf 
gen.main.total = c("Armored Dinoflagellate","Naked Dinoflagellate","Limnothrix","Microcystis","Lindavia",
                   "Cryptomonad", "Asterionella", "Dinobryon", "Fragilaria","Cocconeis",
                   "Flagellated Green", "Cf. Komvophoron / Trichormus","Cyanobacteria","Elakatothrix","Mallomonas",
                   "Segmented Green", "Peanut","Other")


#Asterocapsa-- NOT ENOUGH DATA
#DONT LOOK @ Cyanobacteria in WINTER


gen.ndmi=c("Cryptomonad")

ggplot((subset(totals, Genus %in%gen.ndmi)), aes(chlor.int,log(CellBioVol)))+
  scale_color_viridis_c(option="viridis")+
  geom_point()+
  geom_smooth()+
  labs(title="Peanut")+
  #facet_wrap(~ice.pres, scales='free')
  #scale_color_brewer()+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))


ggplot((subset(totals, ice.pres%in% 0)), aes(chlor.surf,log(CellBioVol)))+
  geom_point(aes(col=frlight))+  #light availablity 
  geom_smooth(se=T)+
  labs(title="")+
  scale_color_viridis_c(option="plasma")+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))
  
scales = "free"
method= "lm"

a =subset(totals, ice.pres%in% 1)$frlight
a
write.csv(a,'wintertotals.csv')

b=subset(totals, ice.pres%in% 0)$frlight
b
write.csv(b,'summertotals.csv')


ggplot((subset(lightextfull, ice.pres %in% 1)), aes(chlor.int,log(CellBioVol)))+
  geom_point(aes(col=extcoef))+  #light availablity 
  geom_smooth(se=T)+
  labs(title="")+
  scale_color_viridis_c(option="plasma")+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs), scales = "free")

(subset(totals, Genus %in%gen.ndmi)


ggplot((subset(totals, Genus %in%gen.ndmi)), aes(chlor.int,log(CellBioVol)))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title="Peanut")+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))



ggplot(+ aes(chlor.int,log(CellBioVol)))+
  geom_point()+
  geom_smooth()+
  labs()+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))

gg <- ggplot(totals.new, aes(x=sampledate, y=log(CellBioVol))) + 
  geom_point(aes(col=chlor.int))+ 
  scale_color_viridis_c(option="viridis")+
  geom_smooth()+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))
plot(gg)

gg <- ggplot(subset(totals.new, Genus %in%gen.ndmi, aes(x=sampledate, y=log(CellBioVol)))) + 
  geom_point(aes(col=chlor.int))+ 
  geom_smooth()+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))
plot(gg)

#need to factor my chlor.int and surf to get readable colors because right now we are on a cont path
#this is for ease of coloring 

totals.new <- 
  totals %>%
  mutate(
    chlor.int= cut(chlor.int,
               breaks = c(-Inf, 1, 3, 5, 8, Inf),
               lables= c("0-1","1-3", "3-5", "5-8","8+")
              )
    )
gen.main.total = c("Armored Dinoflagellate","Naked Dinoflagellate","Limnothrix","Microcystis","Lindavia",
                   "Cryptomonad", "Asterionella", "Dinobryon", "Fragilaria","Cocconeis",
                   "Flagellated Green", "Cf. Komvophoron / Trichormus","Cyanobacteria","Elakatothrix","Mallomonas",
                   "Segmented Green", "Peanut","Other")
gen.ndmi=c("Peanut")

ggplot((subset(totals, Genus %in%gen.ndmi)), aes(sampledate,log(CellBioVol)))+
  geom_point(aes(col=chlor.surf), size=2)+
  geom_smooth(method=lm,se=T)+
  labs(title="Peanut Integrated Chlorophyll")+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))


ggplot((subset(totals, Genus %in%gen.ndmi)), aes(chlor.int,log(CellBioVol)))+
  geom_point(aes(col=frlight))+  #light availablity 
  geom_smooth(method=lm, se=T)+
  labs(title="")+
  scale_color_viridis_c(option="plasma")+
  labs(title="Peanut Integrated Chlorophyll")+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs), scales="free")

scales = "free"
method= "lm"


ggplot((subset(totals)), aes(chlor.int,log(CellBioVol)))+
  geom_point(aes(col=light))+
  geom_smooth(se=T)+
  labs(title="")+
  scale_color_viridis_c(option="viridis")+
  facet_wrap(~ice.pres, labeller=labeller(ice.pres = ice.labs))



(subset(totals, Genus %in%gen.ndmi)


#totals.surf<-
 # totals %>%
  #mutate(
   # chlor.surf= cut(chlor.surf,
    #               breaks = c(-Inf, 1, 3, 5, 8, Inf),
     #              lables= c("0-1","1-3", "3-5", "5-8","8+")
    #)
  #)



########===================

#do the box thing with interesting chems and interesting genera

# paired-plot of predictors and total biovolumes for co-linearity
pairs(totals[,c(4:11,13)])

#split totals into totals_iceon and totals_iceoff,
totals_iceon <- totals

totals$month = month(totals$sampledate)

winter = subset(totals, month < 4)
summer= subset(totals, month>4)


#define seasons
ice = read.csv('data/iceduration.csv', stringsAsFactors = F)
ice %<>% rename(ice.on = datefirstice,
                ice.off = datelastice)

ice$ice.on = mdy(ice$datefirstice)
ice$ice.off = mdy(ice$datelastice)

ice %<>% subset(year>1996 & year <2010)
ice %<>% subset(lakeid == "SP")

ice.labs <- c("Ice Off", "Ice On")
names(ice.labs) <- c(0, 1)

#pull out genera to look at against drivers in seasonal situations 

Limnowinter= subset(winter, Genus=="Limnothrix")
Limnowinter$limno.log.bv = log(Limnowinter$CellBioVol)
limnosummer= subset(summer, Genus=="Limnothrix")
limnosummer$limno.log.bv = log(limnosummer$CellBioVol)

pairs(limnosummer[,c(31:37, 42,45,50)])


Microcystiswinter= subset(winter, Genus=="Microcystis")
Microcystiswinter$Microcystis.log.bv = log(Microcystiswinter$CellBioVol)
Microcystissummer= subset(summer, Genus=="Microcystis")
Microcystissummer$Microcystis.log.bv = log(Microcystissummer$CellBioVol)

pairs(Microcystiswinter[,c(11:16, 18:22,50)])




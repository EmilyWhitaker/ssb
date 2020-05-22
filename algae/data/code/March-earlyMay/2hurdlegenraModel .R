## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz 
library(lubridate) # dealing with dates
library(pscl) #for 2 hurdle modeling


genus = read.csv('data/abiotic_genus_biovolume.csv', stringsAsFactors = F)
genus$sampledate = ymd(genus$sampledate)


totals = read.csv('data/abiotic_total_biovolume.csv',stringsAsFactors = F)
totals$sampledate = mdy(totals$sampledate)

genus = rbind(genus, totals)

genus$one.cbv = genus$CellBioVol+1
genus$log.cbv = log(genus$one.cbv)
genus$int.cbv=as.integer(genus$CellBioVol)
genus$oneint.cbv = genus$int.cbv+1
genus$logint.cbv = log(genus$oneint.cbv)
genus$int.logint.cbv =as.integer(genus$logint.cbv)
genus$int.blueice=as.integer(genus$blueice)
ice.on = subset(genus, ice.pres == 1)
gen.keep = c("Armored Dinoflagellate","Naked Dinoflagellate","Limnothrix","Microcystis","Lindavia",
             "Cryptomonad", "Asterionella", "Dinobryon", "Fragilaria", "Total")
gen.keep1=c("Naked Dinoflagellate")
genus.sub1 = subset(genus, Genus %in% gen.keep1)
ice.on1 = subset(genus.sub1, ice.pres == 1)


#====
genlm1 = lm(CellBioVol ~ wtemp + o2 + ph + light + chlor.int + chlor.surf+ avsnow + whiteice + blueice+ doc, data=ice.on1)
summary(genlm1) #adj r2 0.01018
AIC(genlm1) #4831.011

#remove surf chloro
genlm2 = lm(CellBioVol ~ wtemp + o2 + ph + light + chlor.int + avsnow + whiteice + blueice+ doc, data=ice.on1)
summary(genlm2) #adj r2 0.01664
AIC(genlm2) #5009.62

#remove int chloro
genlm3 = lm(CellBioVol ~ wtemp + o2 + ph + light + avsnow + whiteice + blueice+ doc, data=ice.on1)
summary(genlm3) #adj r2 0.02057
AIC(genlm3) #5007.714

#remove ph
genlm4 = lm(CellBioVol ~ wtemp + o2 + light + avsnow + whiteice + blueice+ doc, data=ice.on1)
summary(genlm4) #adj r2 0.01942
AIC(genlm4) #5007.029

#remove doc
genlm5 = lm(CellBioVol ~ wtemp + o2 + light + avsnow+ whiteice + blueice, data=ice.on1)
summary(genlm5) #adj r2 0.02211
AIC(genlm5) #5005.409 

# remove av snow 
genlm6 = lm(CellBioVol ~  wtemp + o2 + light+ whiteice + blueice, data=ice.on1)
summary(genlm6) #adj r2 0.0261
AIC(genlm6) #5003.469 

# remove white
genlm7 = lm(CellBioVol ~  wtemp + o2 + light + blueice, data=ice.on1)
summary(genlm7) #adj r2 0.02166
AIC(genlm7) #5003.571

# remove blue-- never becomes sig
genlm8 = lm(CellBioVol ~  wtemp + o2 + light, data=ice.on1)
summary(genlm8) #adj r2 0.02166
AIC(genlm8) #5003.571

#
loggenlm1 = lm(log.cbv ~ wtemp + o2 + ph + light + chlor.int+ avsnow + whiteice + blueice+ doc, data=ice.on1)
summary(loggenlm1) #adj r2 -0.005183
AIC(loggenlm1) #975.8097, white ice sig 

#remove light
loggenlm2 = lm(log.cbv ~ wtemp + o2 + ph + chlor.int+ avsnow + whiteice + blueice+ doc, data=ice.on1)
summary(loggenlm2) #adj r2 -0.008308 
AIC(loggenlm2) #1033.892

#remove doc
loggenlm3 = lm(log.cbv ~ wtemp + o2 + ph + chlor.int+ avsnow + whiteice + blueice, data=ice.on1)
summary(loggenlm3) #adj r2 -0.004989  
AIC(loggenlm3) #1032.106

#rmeove ph
loggenlm4 = lm(log.cbv ~ wtemp + o2 + chlor.int+ avsnow + whiteice + blueice, data=ice.on1)
summary(loggenlm4) #adj r2 -0.002687 
AIC(loggenlm4) #1030.566

#remove chlor.int
loggenlm5 = lm(log.cbv ~ wtemp + o2 + avsnow + whiteice + blueice, data=ice.on1)
summary(loggenlm5) #adj r2 0.0001044 
AIC(loggenlm5) #1028.899, blue and white ice sig

#remove water temp
loggenlm6 = lm(log.cbv ~ o2 + avsnow + whiteice + blueice, data=ice.on1)
summary(loggenlm6) #adj r2 0.002742 
AIC(loggenlm6) #1027.264, blue and white ice sig

#remove water temp
loggenlm7 = lm(log.cbv ~ o2 + whiteice + blueice, data=ice.on1)
summary(loggenlm7) #adj r2 0.003694 
AIC(loggenlm7) #1027.264

#doesnt become significant 
loggenlm8 = lm(log.cbv ~whiteice + blueice, data=ice.on1)
summary(loggenlm8) #adj r2 0.002742 
AIC(loggenlm8) #1027.264

#=====

typeof(ice.on1)
class(ice.on1)


nakeddinos.hurdle <- hurdle(int.cbv ~ chlor.surf+ avsnow+ totice+ whiteice+ int.blueice, data = ice.on1)
#error
summary(nakeddinos.hurdle)


lognakeddinos.hurdle <- hurdle(log.cbv ~ ., data = ice.on1)

hurdle(CellBioVol ~ ., data = ice.on1)
summary(mod.hurdle)

## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz 
library(lubridate) # dealing with dates
library(pscl) #for 2 hurdle modeling


#==========
genus = read.csv('data/abiotic_genus_biovolume.csv', stringsAsFactors = F)
genus$sampledate = ymd(genus$sampledate)
write.csv(genus, 'data/genus_check_1.csv', row.names = F)

totals = read.csv('data/abiotic_total_biovolume1.csv',stringsAsFactors = F)
totals$sampledate = mdy(totals$sampledate)

genus = rbind(genus, totals)

#===========
gen.keep = c("Armored Dinoflagellate","Naked Dinoflagellate","Limnothrix","Microcystis","Lindavia",
             "Cryptomonad", "Asterionella", "Dinobryon", "Fragilaria", "Total")

genus.sub = subset(genus, Genus %in% gen.keep)


ggplot(genus.sub, aes(o2, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres)

ggplot(genus.sub, aes(avsnow, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres)

ggplot(genus.sub, aes(whiteice, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres)

ggplot(genus.sub, aes(blueice, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres)

ggplot(genus.sub, aes(whiteice*avsnow, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres)

ggplot(genus.sub, aes(wtemp, log(CellBioVol), color= Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, scales='free')

ggplot(genus.sub, aes(sqrt(chlor.int), log(CellBioVol), color= Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres)

ggplot(genus.sub, aes(log(chlor.surf), log(CellBioVol), color= Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, scales='free')

ggplot(genus.sub, aes(ph, log(CellBioVol), color= Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, scales='free')

ggplot(genus.sub, aes(doc, log(CellBioVol), color= Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, scales='free')

ggplot(genus.sub, aes(sqrt(light), log(CellBioVol), color= Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, scales='free')






#========
#13/13 modeling in winter

genus$one.cbv = genus$CellBioVol+1
genus$log.cbv = log(genus$one.cbv)
ice.on = subset(genus, ice.pres == 1)
gen.keep = c("Armored Dinoflagellate","Naked Dinoflagellate","Limnothrix","Microcystis","Lindavia",
             "Cryptomonad", "Asterionella", "Dinobryon", "Fragilaria", "Total")
gen.keep1=c("Naked Dinoflagellate")
genus.sub1 = subset(genus, Genus %in% gen.keep1)
ice.on1 = subset(genus.sub1, ice.pres == 1)
#======
nakeddinos.hurdle <- hurdle(log.cbv ~ ., data = gen.keep1)

mod.hurdle <- hurdle(CellBioVol ~ ., data = ice.on1)
hurdle(CellBioVol ~ ., data = ice.on1)
summary(mod.hurdle)
#=======
#Naked Dinos 
gen.keep1=c("Naked Dinoflagellate")

genus.sub = subset(genus, Genus %in% gen.keep)

genus.sub1 = subset(genus, Genus %in% gen.keep1)
  
genlm1 = lm(log.cbv ~ wtemp + o2 + ph + light + chlor.int + chlor.surf+ avsnow + whiteice + blueice, data=genus.sub1)
summary(genlm1) #adj r2 -0.0008902 
AIC(genlm1) #7351.195

#remove light
genlm2 = lm(log.cbv ~ wtemp + o2 + ph + chlor.int + chlor.surf+ avsnow + whiteice + blueice, data=genus.sub1)
summary(genlm2) #adj r2 -0.0003415  
AIC(genlm2) #8065.681

#remove chlor.surf
genlm3 = lm(log.cbv ~ wtemp + o2 + ph + chlor.int + avsnow + whiteice + blueice, data=genus.sub1)
summary(genlm3) #adj r2 -0.0005196  
AIC(genlm3) #8065.681

#remove chlor.int 
genlm4 = lm(log.cbv ~ wtemp + o2 + ph + avsnow + whiteice + blueice, data=genus.sub1)
summary(genlm4) #adj r2 -8.95e-05   
AIC(genlm4) #8236.704

#remove av snow
genlm5 = lm(log.cbv ~ chlor.surf, data=genus.sub1)
summary(genlm5) #adj r2 -8.95e-05   
AIC(genlm5) #8236.704

#=====
Limnothrix
gen.keep2=c("Limnothrix")
genus.sub2 = subset(genus, Genus %in% gen.keep2)


Limnlm1 = lm(log.cbv ~ wtemp + o2 + ph + light + chlor.int + chlor.surf+ avsnow + whiteice + blueice, data=genus.sub2)
summary(Limnlm1) #adj r2 -0.0009895
AIC(Limnlm1) #5312.26

#remove cchlor.surf
Limnlm2 = lm(log.cbv ~ wtemp + o2 + ph + light + chlor.int + avsnow + whiteice + blueice, data=genus.sub2)
summary(Limnlm2) #adj r2 -0.0002034 
AIC(Limnlm2) #5396.84

#remove chlor.int
Limnlm3 = lm(log.cbv ~ wtemp + o2 + ph + light  + avsnow + whiteice + blueice, data=genus.sub2)
summary(Limnlm3) #adj r2 -0.0004104 
AIC(Limnlm3) #5445.177

#remove white ice
Limnlm4 = lm(log.cbv ~ wtemp + o2 + ph + light + chlor.int + avsnow + blueice, data=genus.sub2)
summary(Limnlm4) #adj r2 0.0003329
AIC(Limnlm4) #5394.9

#remove add back white ice remove light
Limnlm5 = lm(log.cbv ~ wtemp + o2 + ph + chlor.int + avsnow + whiteice + blueice, data=genus.sub2)
summary(Limnlm5) #adj r2 0.001543 
AIC(Limnlm5) #5999.668

#remove remove blueice
Limnlm6 = lm(log.cbv ~ wtemp + o2 + ph + chlor.int + avsnow + whiteice, data=genus.sub2)
summary(Limnlm6) #adj r2 0.002053 
AIC(Limnlm6) #5997.695

#remove remove wtemp
Limnlm7 = lm(log.cbv ~o2 + ph + chlor.int + avsnow + whiteice, data=genus.sub2)
summary(Limnlm7) #adj r2 0.002509 
AIC(Limnlm7) #5995.826

#remove remove avsnow
Limnlm8 = lm(log.cbv ~ o2 + ph + chlor.int + whiteice, data=genus.sub2)
summary(Limnlm8) #adj r2 0.002891 
AIC(Limnlm8) #5994.097

#remove remove ph
Limnlm9 = lm(log.cbv ~ o2, data=genus.sub2)
summary(Limnlm9) #adj r2 0.003236 
AIC(Limnlm9) #[1] 6032.415

#====
gen.keep1=c("Naked Dinoflagellate")

genus.sub1 = subset(genus, Genus %in% gen.keep1)

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



#======

gen.keep3=c("Microcystis")
genus.sub3 = subset(genus, Genus %in% gen.keep3)

######
gen.nine = c("Asterocapsa","Cocconeis","Flagellated Green", "Cf. Komvophoron / Trichormus","Cyanobacteria","Elakatothrix","Mallomonas",
             "Segmented Green", "Peanut", "Total")

genus.sub2 = subset(genus, Genus %in% gen.nine)

ggplot(genus.sub2, aes(o2, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, scales='free')

ggplot(genus.sub2, aes(avsnow, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres,scales='free')

ggplot(genus.sub2, aes(whiteice, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, scales = 'free')

ggplot(genus.sub2, aes(blueice, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, scales= 'free')

ggplot(genus.sub2, aes(whiteice*avsnow, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, scales= 'free')

ggplot(genus.sub2, aes(wtemp, log(CellBioVol), color= Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, scales='free')

ggplot(genus.sub2, aes(sqrt(chlor.int), log(CellBioVol), color= Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres)

ggplot(genus.sub2, aes(log(chlor.surf), log(CellBioVol), color= Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, scales='free')

ggplot(genus.sub2, aes(ph, log(CellBioVol), color= Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, scales='free')

ggplot(genus.sub2, aes(doc, log(CellBioVol), color= Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, scales='free')

ggplot(genus.sub2, aes(sqrt(light), log(CellBioVol), color= Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~ice.pres, scales='free')




#=====
genus
write.csv(genus, 'data/genus_check.csv', row.names = F)

$one.cbv = genus$CellBioVol+1
genus$log.cbv = log(genus$one.cbv)
ice.off = subset(genus, ice.pres == 0)
gen.keep = c("Armored Dinoflagellate","Naked Dinoflagellate","Limnothrix","Microcystis","Lindavia",
             "Cryptomonad", "Asterionella", "Dinobryon", "Fragilaria")
genus.sub1 = subset(ice.off, Genus %in% gen.keep)


#gen.keep = c("Armored Dinoflagellate","Naked Dinoflagellate","Limnothrix","Microcystis","Lindavia",
 #            "Cryptomonad", "Asterionella", "Dinobryon", "Fragilaria", "Total")
#genus.sub = subset(genus, Genus %in% gen.keep)


ggplot(genus.sub1, aes(o2sat, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(cond, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(frlight, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(chlor.int, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(phaeo, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(ph, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(phair, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(alk, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(dic, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(toc, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(no3no2, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(nh4, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(totnf, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(totpf, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(sqrt(totpuf), log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(drsif, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(tpm, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(cl, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(so4, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(ca, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(mg, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(na, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(k, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(fe, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(mn, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(year4frac, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes((chlor.surf), log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(genus.sub1, aes(light, log(CellBioVol),color=Genus))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

#=======
genus$one.cbv = genus$CellBioVol+1
genus$log.cbv = log(genus$one.cbv)
ice.off = subset(genus, ice.pres == 0)
gen.keep = c("Armored Dinoflagellate","Naked Dinoflagellate","Limnothrix","Microcystis","Lindavia",
             "Cryptomonad", "Asterionella", "Dinobryon", "Fragilaria")
gen.nd = c("Naked Dinoflagellate")
genus.sub.nd = subset(ice.off, Genus %in% gen.nd) #multiple data points per date



# begin model for ice-off ND
nd.off.lm1 = lm(log.cbv ~ wtemp + log(chlor.surf)+ o2 + cond+ phaeo+ ph+ dic+ tic+ 
               doc+ no3no2 + nh4+ totpf+ sqrt(totpuf)+
               drsif+ brsif+ so4+ ca+ mg+ na+ fe+ light, data=genus.sub.nd)
summary(nd.off.lm1) #essentially perfect fit: summary may be unreliable, adj r2 = 1
AIC(nd.off.lm1) # -6.25   

## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz 
library(lubridate) # dealing with dates
library(pscl) #for 2 hurdle modeling

#=========
totals_and_genus = read.csv('data/clean_abiotic_genus_03262020.csv', stringsAsFactors = F)
totals_and_genus$sampledate = mdy(totals_and_genus$sampledate)

#add those cols 
totals_and_genus$one.cbv = totals_and_genus$CellBioVol+1
totals_and_genus$log.cbv = log(totals_and_genus$one.cbv)
totals_and_genus$int.cbv=as.integer(totals_and_genus$CellBioVol)
totals_and_genus$oneint.cbv = totals_and_genus$int.cbv+1
totals_and_genus$logint.cbv =as.integer(log(totals_and_genus$oneint.cbv))

#gen.keep = c( "Fragilaria")

#=====
#Fragilaria during ice-on

ice.on = subset(totals_and_genus, ice.pres == 1)
gen.keep1=c("Fragilaria")
genus.sub1 = subset(totals_and_genus, Genus %in% gen.keep1)
Frag.ice.on = subset(genus.sub1, ice.pres == 1) #correct

hist(Frag.ice.on$log.cbv) #slightly__zero-inflated

#===== 
#Linear graphs Ice on
ggplot(Frag.ice.on, aes(wtemp, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(o2, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(o2sat, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(cond, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(frlight, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(chlor.int, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(phaeo, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(ph, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(phair, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(alk, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(dic, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(tic, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(doc, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(toc, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(no3no2, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(no2, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(nh4, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(totnf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(totnuf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(totpf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(totpuf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(drsif, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(brsif, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(brsiuf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(tpm, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(cl, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(so4, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(ca, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(mg, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(na, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(k, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(fe, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(mn, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(chlor.surf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(avsnow, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(totice, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(whiteice, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(blueice, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(light, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.on, aes(iceduration, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

#=======
#Fragilaria ice off

ice.off = subset(totals_and_genus, ice.pres == 0)
gen.keep1=c("Fragilaria")
genus.sub1 = subset(totals_and_genus, Genus %in% gen.keep1)
Frag.ice.off = subset(genus.sub1, ice.pres == 0) #correct

hist(Frag.ice.off$log.cbv)  

#=======
#Linear graphs Ice off
ggplot(Frag.ice.off, aes(wtemp, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(o2, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(o2sat, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(cond, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(frlight, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(chlor.int, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(phaeo, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(ph, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(phair, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(alk, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(dic, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(tic, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(doc, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(toc, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(no3no2, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(no2, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(nh4, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(totnf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(totnuf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(totpf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(totpuf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(drsif, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(brsif, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(brsiuf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(tpm, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(cl, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(so4, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(ca, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(mg, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(na, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(k, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(fe, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(mn, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(chlor.surf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(light, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Frag.ice.off, aes(iceduration, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

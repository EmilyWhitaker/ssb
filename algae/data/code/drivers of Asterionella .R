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

#gen.keep = c("Asterionella", "Dinobryon", "Fragilaria")

#=====
#Asterionella during ice-on

ice.on = subset(totals_and_genus, ice.pres == 1)
gen.keep1=c("Asterionella")
genus.sub1 = subset(totals_and_genus, Genus %in% gen.keep1)
Aster.ice.on = subset(genus.sub1, ice.pres == 1) #correct

hist(Aster.ice.on$log.cbv) #NOPE zero-inflated!!!!!!

#===== 
#Linear graphs Ice on
ast_wtemp<- ggplot(Aster.ice.on, aes(wtemp, log.cbv))+
  #geom_point(size=4)+
  geom_point()+
  geom_smooth(method=lm, se=TRUE)
  scale_color_brewer(palette = 'Paired')
ast_wtemp  
ggsave(plot= ast_wtemp, filename ='figures/AsterionellaWTemp.png',height=18, width = 16, units = 'in')

#p3 <- ggplot(data, aes(x=my_x, y=my_y)) +
#  geom_point() +
#  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
#  theme_ipsum()

ast_o2<- ggplot(Aster.ice.on, aes(o2, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=TRUE)+
  scale_color_brewer(palette = 'Paired')
ast_o2
ggsave(plot= ast_o2, filename ='figures/Asterionellao2.png',height=18, width = 16, units = 'in')

ast_o2sat<- ggplot(Aster.ice.on, aes(o2sat, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=TRUE)+
  scale_color_brewer(palette = 'Paired')
ast_o2sat
ggsave(plot= ast_o2sat, filename ='figures/Asterionellao2sat.png',height=18, width = 16, units = 'in')


ast_cond<-ggplot(Aster.ice.on, aes(cond, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=TRUE)+
  scale_color_brewer(palette = 'Paired')
ast_cond
ggsave(plot= ast_cond, filename ='figures/Asterionellacond.png',height=18, width = 16, units = 'in')

ast_frlight<-ggplot(Aster.ice.on, aes(frlight, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=TRUE, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')
ast_frlight
ggsave(plot= ast_frlight, filename ='figures/Asterionellafrlight.png',height=18, width = 16, units = 'in')

ast_chlor_int<- ggplot(Aster.ice.on, aes(chlor.int, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_chlor_int
ggsave(plot= ast_chlor_int, filename ='figures/AsterionellaChlorInt.png',height=18, width = 16, units = 'in')

ast_phaeo<- ggplot(Aster.ice.on, aes(phaeo, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_phaeo
ggsave(plot= ast_phaeo, filename ='figures/AsterionellaPhaeo.png',height=18, width = 16, units = 'in')

ast_ph<-ggplot(Aster.ice.on, aes(ph, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_ph
ggsave(plot= ast_ph, filename ='figures/AsterionellaPh.png',height=18, width = 16, units = 'in')


ast_phair<-ggplot(Aster.ice.on, aes(phair, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_phair
ggsave(plot= ast_phair, filename ='figures/AsterionellaPhair.png',height=18, width = 16, units = 'in')

ast_alk<-ggplot(Aster.ice.on, aes(alk, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_alk
ggsave(plot= ast_alk, filename ='figures/AsterionellaAlk.png',height=18, width = 16, units = 'in')

ast_dic<-ggplot(Aster.ice.on, aes(dic, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_dic

ast_tic<-ggplot(Aster.ice.on, aes(tic, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_tic

ast_doc<-ggplot(Aster.ice.on, aes(doc, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_doc

ast_toc<-ggplot(Aster.ice.on, aes(toc, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_toc

ast_no3no2<-ggplot(Aster.ice.on, aes(no3no2, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_no3no2

#no values
#ast_no2<-ggplot(Aster.ice.on, aes(no2, log.cbv))+
#  geom_point()+
#  geom_smooth(method='lm',se=F, aes(group=Genus))+
#  scale_color_brewer(palette = 'Paired')
#ast_no2

ast_nh4<-ggplot(Aster.ice.on, aes(nh4, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_nh4

ast_totnf<-ggplot(Aster.ice.on, aes(totnf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_totnf

ast_totnuf<-ggplot(Aster.ice.on, aes(totnuf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_totnuf

ast_totpf<-ggplot(Aster.ice.on, aes(totpf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_totpf

ast_totpuf<-ggplot(Aster.ice.on, aes(totpuf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_totpuf

ast_drsif<-ggplot(Aster.ice.on, aes(drsif, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_drsif

ast_brsif<-ggplot(Aster.ice.on, aes(brsif, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_brsif

ast_brsiuf<-ggplot(Aster.ice.on, aes(brsiuf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_brsiuf

#no vaules 
#ast_tpm<-ggplot(Aster.ice.on, aes(tpm, log.cbv))+
#  geom_point()+
#  geom_smooth(method='lm',se=T)+
#  scale_color_brewer(palette = 'Paired')
#ast_tpm

ast_cl<-ggplot(Aster.ice.on, aes(cl, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_cl

ast_so4<-ggplot(Aster.ice.on, aes(so4, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_so4

ast_ca<-ggplot(Aster.ice.on, aes(ca, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_ca

ast_mg<-ggplot(Aster.ice.on, aes(mg, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_mg

ast_na<-ggplot(Aster.ice.on, aes(na, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_na

ast_k<-ggplot(Aster.ice.on, aes(k, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_k

ast_fe<-ggplot(Aster.ice.on, aes(fe, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_fe

ast_mn<-ggplot(Aster.ice.on, aes(mn, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_mn

ast_chlorsurf<-ggplot(Aster.ice.on, aes(chlor.surf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_chlorsurf

ast_avsnow<-ggplot(Aster.ice.on, aes(avsnow, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_avsnow

ast_totice<-ggplot(Aster.ice.on, aes(totice, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_totice

ast_whiteice<-ggplot(Aster.ice.on, aes(whiteice, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_whiteice

ast_blueice<-ggplot(Aster.ice.on, aes(blueice, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_blueice

ast_light<-ggplot(Aster.ice.on, aes(light, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_light

ast_iceduration<-ggplot(Aster.ice.on, aes(iceduration, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')
ast_iceduration

#=======
#Asterionella ice off

ice.off = subset(totals_and_genus, ice.pres == 0)
gen.keep1=c("Asterionella")
genus.sub1 = subset(totals_and_genus, Genus %in% gen.keep1)
Aster.ice.off = subset(genus.sub1, ice.pres == 0) #correct

hist(Aster.ice.off$log.cbv) #zero inflated  

#=======
#Linear graphs Ice off
ggplot(Aster.ice.off, aes(wtemp, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')


ggplot(Aster.ice.off, aes(o2, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(o2sat, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(cond, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(frlight, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(chlor.int, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=T)+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(phaeo, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(ph, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(phair, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(alk, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(dic, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(tic, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(doc, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(toc, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(no3no2, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(no2, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(nh4, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(totnf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(totnuf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(totpf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(totpuf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(drsif, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(brsif, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(brsiuf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(tpm, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(cl, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(so4, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(ca, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(mg, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(na, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(k, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(fe, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(mn, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(chlor.surf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(light, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(Aster.ice.off, aes(iceduration, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

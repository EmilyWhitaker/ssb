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

#========
#Naked Dinoflagellate during ice-off

ice.off = subset(totals_and_genus, ice.pres == 0)
gen.keep1=c("Naked Dinoflagellate")
genus.sub1 = subset(totals_and_genus, Genus %in% gen.keep1)
ND.ice.off = subset(genus.sub1, ice.pres == 0) #correct

#modeling ND.ice.off

ND.lm1 = lm(log.cbv ~ wtemp + log(chlor.surf)+ o2 + cond + totpf+ sqrt(totpuf)+
                 iceduration, data=ND.ice.off)
summary(ND.lm1) #0.3178, wtemp is sig
AIC(ND.lm1) #183.9418

#remove o2
ND.lm2 = lm(log.cbv ~ wtemp + log(chlor.surf) + cond + totpf+ sqrt(totpuf)+
              iceduration, data=ND.ice.off)
summary(ND.lm2) #0.3382
AIC(ND.lm2) #182.2768

#remove cond
ND.lm3 = lm(log.cbv ~ wtemp + log(chlor.surf)  + totpf+ sqrt(totpuf)+
              iceduration, data=ND.ice.off)
summary(ND.lm3) #0.2616 -- more things significant: wtemp, chlor.surf, weak totpf, totpuf 
AIC(ND.lm3) #286.9145

# add cond back in 
ND.lm4 = lm(log.cbv ~ wtemp + log(chlor.surf) + totpf+ sqrt(totpuf)+ cond +
              iceduration, data=ND.ice.off)
summary(ND.lm4) #0.3382 -- 
AIC(ND.lm4) #182.2768

# add dic back in 
ND.lm5 = lm(log.cbv ~ wtemp + log(chlor.surf), data=ND.ice.off)
summary(ND.lm5) #0.3382 -- 
AIC(ND.lm5) #182.2768



nd.off.lm1 = lm(log.cbv ~ wtemp + log(chlor.surf)+ o2 + cond+ phaeo+ ph+ dic+ tic+ 
                  doc+ no3no2 + nh4+ totpf+ sqrt(totpuf)+
                  drsif+ brsif+ so4+ ca+ mg+ na+ fe+ light, data=genus.sub.nd)
summary(nd.off.lm1) #essentially perfect fit: summary may be unreliable, adj r2 = 1
AIC(nd.off.lm1)

##======
#Naked Dinoflagellate during ice-on

ice.on = subset(totals_and_genus, ice.pres == 1)
gen.keep1=c("Naked Dinoflagellate")
genus.sub1 = subset(totals_and_genus, Genus %in% gen.keep1)
ND.ice.on = subset(genus.sub1, ice.pres == 1) #correct


NDon.lm1 = lm(log.cbv ~ wtemp+ avsnow+ whiteice +blueice+ chlor.int + chlor.surf+ o2 + cond+ totpuf, data=ND.ice.on)
summary(NDon.lm1) #0.9072,
AIC(NDon.lm1) #36.12653

#remove cond
NDon.lm2 = lm(log.cbv ~ wtemp+ avsnow+ whiteice +blueice+ chlor.int + chlor.surf+ o2 + totpuf, data=ND.ice.on)
summary(NDon.lm2) #0.9558, everything but avsnow has some sig 
AIC(NDon.lm2) #38.10273

#snow/white ice interaction
NDon.lm3 = lm(log.cbv ~ wtemp+ avsnow*whiteice +blueice+ chlor.int + chlor.surf+ o2 + totpuf, data=ND.ice.on)
summary(NDon.lm3) #0.971 ,  interaction is .2498
AIC(NDon.lm3) #30.17392

#remove totpuf
NDon.lm4 = lm(log.cbv ~ wtemp+ avsnow*whiteice +blueice+ chlor.int + chlor.surf+ o2 , data=ND.ice.on)
summary(NDon.lm4) #0.8154 ,  interaction is .4205- others lost sig values besides chlor.surf 
AIC(NDon.lm4) #55.24354
plot(NDon.lm4)

#add totpuf back, remove av snow
NDon.lm5 = lm(log.cbv ~ wtemp+ whiteice +blueice+ chlor.int + chlor.surf+ o2+totpuf , data=ND.ice.on)
summary(NDon.lm5) #0.9633 ,  all factors sig 
AIC(NDon.lm5) #37.29873
plot(NDon.lm5)


#======
ice.off = subset(totals_and_genus, ice.pres == 0)
gen.keep2=c("Limnothrix")
genus.sub2 = subset(totals_and_genus, Genus %in% gen.keep2)
LX.ice.off = subset(genus.sub2, ice.pres == 0) #correct

hist(LX.ice.off$log.cbv) #zeroinflated


##====
#Linear graphs Ice on
ggplot(LX.ice.on, aes(cond, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F, aes(group=Genus))+
  scale_color_brewer(palette = 'Paired')

ggplot(LX.ice.on, aes(iceduration, log.cbv))+
  geom_point()+
  geom_smooth(method='lm',se=F)+
  scale_color_brewer(palette = 'Paired')



#====
ice.on = subset(totals_and_genus, ice.pres == 1)
gen.keep2=c("Limnothrix")
genus.sub2 = subset(totals_and_genus, Genus %in% gen.keep2)
LX.ice.on = subset(genus.sub2, ice.pres == 1) #correct
write.csv(LX.ice.on, 'data/lx_factors.csv', row.names = F)

hist(LX.ice.on$log.cbv) #zeroinflated

LXon.lm1 = lm(log.cbv ~ wtemp+ avsnow +blueice+ chlor.int + chlor.surf+ o2 + cond+ totpuf+ o2sat, data=LX.ice.on)
summary(LXon.lm1) #0.9721,
AIC(LXon.lm1) #10.06639 

#remove blueice
LXon.lm2 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + cond+ totpuf+ o2sat, data=LX.ice.on)
summary(LXon.lm2) #0.9855,
AIC(LXon.lm2) #8.535665 
plot(LXon.lm2)

# add iceduration
LXon.lm3 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + cond+ totpuf+ o2sat+iceduration, data=LX.ice.on)
summary(LXon.lm3) #0.9855,
AIC(LXon.lm3) #8.535665 
plot(LXon.lm3)

#remove cond
LXon.lm4 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+iceduration, data=LX.ice.on)
summary(LXon.lm4) #0.7312,
AIC(LXon.lm4) #49.51664 
plot(LXon.lm4)

#remove iceduration 
LXon.lm5 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat, data=LX.ice.on)
summary(LXon.lm5) #0.7828,
AIC(LXon.lm5) #48.41207 
plot(LXon.lm4)

#add light 
LXon.lm6 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+light, data=LX.ice.on)
summary(LXon.lm6) #0.3041,
AIC(LXon.lm6) #42.24704 
plot(LXon.lm4)

#remove light add frlight
LXon.lm7 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+frlight, data=LX.ice.on)
summary(LXon.lm7) #0.3041,
AIC(LXon.lm7) #42.24704 
plot(LXon.lm4)

#remove frlight add phaeo
LXon.lm7 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+phaeo, data=LX.ice.on)
summary(LXon.lm7) #0.8491, phaeo not sig 0.19526
AIC(LXon.lm7) #42.58499 

#remove phaeo add ph
LXon.lm8 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+ph, data=LX.ice.on)
summary(LXon.lm8) #0.9272, ph almost sig 0.05817
AIC(LXon.lm8) #33.84211 

#remove doc add cl YES
LXon.lm13 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+cl, data=LX.ice.on)
summary(LXon.lm13) #0.9654, sig 0.03386
AIC(LXon.lm13) #18.07168
plot(LXon.lm13)

#remove cl add no3no2 yes
LXon.lm15 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+ no3no2, data=LX.ice.on)
summary(LXon.lm15) #0.9857, 0.004720
AIC(LXon.lm15) #14.30095
plot(LXon.lm15)

#remove cl add so4 Meh
LXon.lm14 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+ ca, data=LX.ice.on)
summary(LXon.lm14) #0.9356,  0.0640
AIC(LXon.lm14) #24.90518


#remove mg add na-- ummmmmmm
LXon.lm14 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+ na, data=LX.ice.on)
summary(LXon.lm14) #0.9176,  0.0826
AIC(LXon.lm14) #27.60904
#======
#remove ph add phair
LXon.lm9 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+phair, data=LX.ice.on)
summary(LXon.lm9) #0.7158, not sig 0.8258
AIC(LXon.lm9) #50.18397 

#remove phair add alk
LXon.lm10 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+alk, data=LX.ice.on)
summary(LXon.lm10) #0.8895, not sig 0.1127
AIC(LXon.lm10) #30.84621 

#remove alk add dic NO
LXon.lm11 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+dic, data=LX.ice.on)
summary(LXon.lm11) #0.7906, not sig 0.1127
AIC(LXon.lm11) #46.51692

#remove dic add tic NO
LXon.lm12 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+tic, data=LX.ice.on)
summary(LXon.lm12) #0.7797, not sig 0.4028
AIC(LXon.lm12) #47.12683

#remove dic add tic NO
LXon.lm13 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+toc, data=LX.ice.on)
summary(LXon.lm13) #0.7855, not sig 0.4028
AIC(LXon.lm13) #46.80916

#remove tic add doc NO
LXon.lm13 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+doc, data=LX.ice.on)
summary(LXon.lm13) #0.7849, not sig 0.4028
AIC(LXon.lm13) #46.80916

#remove na add k-- 
LXon.lm15 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+ k, data=LX.ice.on)
summary(LXon.lm15) #0.869,  0.1352
AIC(LXon.lm15) #32.71706

#remove k add fe--NO 
LXon.lm15 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+ fe, data=LX.ice.on)
summary(LXon.lm15) #0.7178,  0.796
AIC(LXon.lm15) #50.09732

#remove fe add mn--NO 
LXon.lm15 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+ mn, data=LX.ice.on)
summary(LXon.lm15) #0.7641,  0.4689
AIC(LXon.lm15) #47.94772

#remove so4 add mg 
LXon.lm14 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+ mg, data=LX.ice.on)
summary(LXon.lm14) #0.773,  0.2494
AIC(LXon.lm14) #38.76229

#remove cl add so4 NO
LXon.lm14 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+ so4, data=LX.ice.on)
summary(LXon.lm14) #0.4809, 
AIC(LXon.lm14) #47.85963

#remove cl add nh4 no
LXon.lm16 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+ nh4, data=LX.ice.on)
summary(LXon.lm16) #0.8742, 0.14260
AIC(LXon.lm16) #40.4084

#remove nh4 add totnf no
LXon.lm16 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+ totnf, data=LX.ice.on)
summary(LXon.lm16) #0.7374, 0.6168
AIC(LXon.lm16) #49.23407

#remove totnf add totnuf no
LXon.lm16 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+ totnuf, data=LX.ice.on)
summary(LXon.lm16) #0.7212, 0.7553
AIC(LXon.lm16) #49.95457

#remove totpf add drsif no
LXon.lm16 = lm(log.cbv ~ wtemp+ avsnow + chlor.int + chlor.surf+ o2 + totpuf+ o2sat+ drsif, data=LX.ice.on)
summary(LXon.lm16) #0.7164, 0.8159
AIC(LXon.lm16) #50.1568
#====







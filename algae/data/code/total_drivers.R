## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz 
library(lubridate) # dealing with dates

#==========

# load abiotic total biovolume data

totals = read.csv('data/abiotic_total_biovolume.csv',stringsAsFactors = F)
totals$sampledate = ymd(totals$sampledate)
totals$log.cbv = log(totals$CellBioVol)

# look at tbv over time
hist(totals$CellBioVol)
hist(log(totals$CellBioVol))
shapiro.test(log(totals$CellBioVol))

ggplot(totals, aes(year, log(CellBioVol)))+
  geom_point()+
  geom_smooth(method='lm')


# potential predictor variables: wtemp, chlor.int, chlor.surf, o2, ph, doc, avsnow, totice, whiteice, blueice, light, frlight 

pairs(totals[,c(2,3,6,7,14,37:42,47)])


# water temp vs cbv - similar negative relationship throughout, steeper in ice-off
hist(totals$wtemp)
ggplot(totals, aes(wtemp, log.cbv))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ice.pres, scales='free')

# chlor.int vs cbv - positive predictor during winter
hist(totals$chlor.int)
hist(sqrt(totals$chlor.int))
shapiro.test(sqrt(totals$chlor.int))

ggplot(totals, aes(sqrt(chlor.int), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~ice.pres, scales='free')

# chlor.surf vs cbv - positive predictor during summer 
hist(totals$chlor.surf)
hist(log(totals$chlor.surf))
shapiro.test(log(totals$chlor.surf))

ggplot(totals, aes(log(chlor.surf), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~ice.pres, scales='free')

# o2 vs cbv - positive relationship for both ice on and off, steeper in winter
hist(totals$o2)
shapiro.test(totals$o2)

ggplot(totals, aes(o2, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~ice.pres, scales='free')

# ph vs cbv - ph is positive, important in winter, not so much in summer 
hist(totals$ph)
shapiro.test(totals$ph)

ggplot(totals, aes(ph, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~ice.pres, scales="free")

# doc vs cbv - not much of a relationship regardless of season
hist(totals$doc)
shapiro.test(totals$doc)

ggplot(totals, aes(doc, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~ice.pres, scales="free")


# avsnow vs cbv - negative relationship during ice on
ice.on = subset(totals, ice.pres == 1)

hist(ice.on$avsnow)
shapiro.test(ice.on$avsnow)

ggplot(totals, aes(avsnow, log.cbv))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ice.pres, scales="free")


# totice vs cbv - potentially non-linear relationship, but no major relationship
hist(ice.on$totice)
shapiro.test(ice.on$totice)

ggplot(totals, aes(totice, log.cbv))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ice.pres, scales="free")


# whiteice vs cbv - negative relationship between white ice 
hist(ice.on$whiteice)
shapiro.test(ice.on$whiteice)

ggplot(totals, aes(whiteice*avsnow, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~ice.pres, scales="free")

# blueice vs cbv - positive relationship between blue ice 
hist(ice.on$blueice)
shapiro.test(ice.on$blueice)

ggplot(totals, aes(blueice, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~ice.pres, scales="free")

# light vs cbv - very important in winter, not as much in ice off 
# - potentially other factors (trophic interactions) limit/explain variance during summer months
hist(sqrt(totals$light))
shapiro.test(totals$light)

ggplot(totals, aes(sqrt(light), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~ice.pres, scales= "free")

# frlight vs cbv - both negative relationships during summer and winter 
hist(sqrt(totals$frlight))
shapiro.test(sqrt(totals$frlight))

ggplot(totals, aes(frlight, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~ice.pres)

#=======
# begin model for ice-on first

lm1 = lm(log.cbv ~ wtemp + o2 + ph + light + chlor.int + avsnow + whiteice + blueice, data=ice.on)
summary(lm1)
AIC(lm1) # -6.25

# get rid of chlor.int
lm2 = lm(log.cbv ~ wtemp + o2 + ph + light + avsnow + whiteice + blueice, data=ice.on)
summary(lm2)
AIC(lm2) # -7.17

# get rid of light 
lm3 = lm(log.cbv ~ wtemp + o2 + ph + chlor.int + avsnow + whiteice + blueice, data=ice.on)
summary(lm3)
AIC(lm3) # 34.03

# get rid of pH
lm4 = lm(log.cbv ~ wtemp + o2 + chlor.int + avsnow + whiteice + blueice, data=ice.on)
summary(lm4)
AIC(lm4) # 32.03

# get rid of chlor.int
lm5 = lm(log.cbv ~ wtemp + o2 + avsnow + whiteice + blueice, data=ice.on)
summary(lm5) # adj R-sq = 0.78
AIC(lm5) # 32.00
# best so far 

# get rid of wtemp
lm6 = lm(log.cbv ~ o2 + avsnow + whiteice + blueice, data=ice.on)
summary(lm6) # adj R-sq = 0.74
AIC(lm6) # 34.4


# add interaction between wtemp and o2
lm7 = lm(log.cbv ~ wtemp*o2 + avsnow + whiteice + blueice, data=ice.on)
summary(lm7) # adj R-sq = 0.76
AIC(lm7) # 33.7
# neither sig, interaction not sig, not better model 

# try interaction between avsnow and whiteice
lm8 = lm(log.cbv ~ wtemp + o2 + avsnow*whiteice + blueice, data=ice.on)
summary(lm8) # adj R-sq = 0.95
AIC(lm8) # 8.51
# best so far 

# ph still insig
lm9 = lm(log.cbv ~ wtemp + o2 + ph + avsnow*whiteice + blueice, data=ice.on)
summary(lm9)
AIC(lm9) # 8.35

# remove wtemp
lm10 = lm(log.cbv ~ o2 + avsnow*whiteice + blueice, data=ice.on)
summary(lm10) # adj R-sq = 0.95
AIC(lm10) # 9.23
# lm10 is best model! - predictors include o2, avsnow*whiteice, blueice 
plot(lm10)


lm11 = lm(log.cbv ~ avsnow*whiteice, data=ice.on)
summary(lm11) # adj R-sq = 0.95
AIC(lm11)
plot(lm)
#===================================
ice.off = subset(totals, ice.pres == 0)

pairs(ice.off[,c(2:8,43, 48)]) #log(frlight) maybe log wtemp
pairs(ice.off[,c(9:16,43, 48)])                
pairs(ice.off[,c(18:25,43, 48)])     #alk, dic/tic  
pairs(ice.off[,c(26:31,43, 48)]) 
pairs(ice.off[,c(32,33,34, 37,42,43,46,48)])


# chlor.surf vs cbv - positive predictor during summer 
hist(ice.off$chlor.surf)
hist(log(ice.off$chlor.surf))
shapiro.test(log(ice.off$chlor.surf))

ggplot(ice.off, aes(log(chlor.surf), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# wtemp vs cbv neg predictor
hist(ice.off$wtemp)
hist(log(ice.off$wtemp))

ggplot(ice.off, aes(wtemp, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# o2 vs cbv pos predictor
hist(ice.off$o2)
hist(log(ice.off$o2))

ggplot(ice.off, aes(o2, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# o2sat vs cbv pos predictor
hist(ice.off$o2sat)
hist(log(ice.off$o2sat))

ggplot(ice.off, aes(o2sat, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# cond vs cbv weak neg 
hist(ice.off$cond)
hist(log(ice.off$cond))

ggplot(ice.off, aes(cond, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

ggplot(ice.off, aes(log(cond), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# frlight vs cbv weak neg 
hist(ice.off$frlight)
hist(log(ice.off$frlight))

ggplot(ice.off, aes(frlight, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# log.chlor.int vs cbv #no change 
hist(ice.off$chlor.int)
hist(log(ice.off$chlor.int))
hist(sqrt(ice.off$chlor.int))

ggplot(ice.off, aes(log(chlor.int), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# log(phaeo) vs cbv - no 
hist(ice.off$phaeo)
hist(log(ice.off$phaeo))
hist(sqrt(ice.off$phaeo))

ggplot(ice.off, aes(log(phaeo), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# ph vs cbv - small decrease 
hist(ice.off$ph)
hist(log(ice.off$ph))
hist(sqrt(ice.off$ph))

ggplot(ice.off, aes(ph, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# phair vs cbv - no real change
hist(ice.off$phair)
hist(log(ice.off$phair))
hist(sqrt(ice.off$phair))

ggplot(ice.off, aes(phair, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# alk vs cbv - no real change
hist(ice.off$alk)
hist(log(ice.off$alk))
hist(sqrt(ice.off$alk))

ggplot(ice.off, aes(alk, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# dic vs cbv - small increase
hist(ice.off$dic)
hist(log(ice.off$dic))
hist(sqrt(ice.off$dic))

ggplot(ice.off, aes(dic, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# tic vs cbv -  increase
hist(ice.off$tic)
hist(log(ice.off$tic))
hist(sqrt(ice.off$tic))

ggplot(ice.off, aes(tic, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')


# doc vs cbv -  minor decrease
hist(ice.off$doc)
hist(log(ice.off$doc))
hist(sqrt(ice.off$doc))

ggplot(ice.off, aes((doc), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# toc vs cbv - minor decrease
hist(ice.off$toc)
hist(log(ice.off$toc))
hist(sqrt(ice.off$toc))

ggplot(ice.off, aes((toc), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# no3no2 vs cbv -  log(no3no2) increase
hist(ice.off$no3no2)
hist(log(ice.off$no3no2))
hist(sqrt(ice.off$no3no2))

ggplot(ice.off, aes(log(no3no2), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# nh4 vs cbv -  log(nh4) decrease
hist(ice.off$nh4)
hist(log(ice.off$nh4))
hist(sqrt(ice.off$nh4))

ggplot(ice.off, aes(sqrt(nh4), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# totnf vs cbv -  no change 
hist(ice.off$totnf)
hist(log(ice.off$totnf))
hist(sqrt(ice.off$totnf))

ggplot(ice.off, aes(sqrt(totnf), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# totnuf vs cbv -  no change 
hist(ice.off$totnuf)
hist(log(ice.off$totnuf))
hist(sqrt(ice.off$totnuf))

ggplot(ice.off, aes(sqrt(totnuf), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# totpf vs cbv -  sqrt(totpf) increase  
hist(ice.off$totpf)
hist(log(ice.off$totpf))
hist(sqrt(ice.off$totpf))

ggplot(ice.off, aes(sqrt(totpf), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# totpuf vs cbv -  sqrt(totpuf) increase  
hist(ice.off$totpuf)
hist(log(ice.off$totpf))
hist(sqrt(ice.off$totpuf))

ggplot(ice.off, aes(sqrt(totpuf), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')


# drsif vs cbv -  sqrt(drsif) increase  
hist(ice.off$drsif)
hist(log(ice.off$drsif))
hist(sqrt(ice.off$drsif))

ggplot(ice.off, aes(sqrt(drsif), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# brsiuf vs cbv -  brsiuf decrease  
hist(ice.off$brsiuf)
hist(log(ice.off$brsiuf))
hist(sqrt(ice.off$brsiuf))

ggplot(ice.off, aes(brsiuf, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# tpm vs cbv -  tpm level
hist(ice.off$tpm)
hist(log(ice.off$tpm))
hist(sqrt(ice.off$tpm))

ggplot(ice.off, aes(sqrt(tpm), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# cl vs cbv -  cl level
hist(ice.off$cl)
hist(log(ice.off$cl))
hist(sqrt(ice.off$cl))

ggplot(ice.off, aes(sqrt(cl), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

# so4 vs cbv -  log(so4) increase
hist(ice.off$so4)
hist(log(ice.off$so4))
hist(sqrt(ice.off$so4))

ggplot(ice.off, aes(log(so4), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

#ca vs cbv -  log(ca) deacrease
hist(ice.off$ca)
hist(log(ice.off$ca))
hist(sqrt(ice.off$ca))

ggplot(ice.off, aes(log(ca), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

#mg vs cbv -  log(mg) deacrease
hist(ice.off$mg)
hist(log(ice.off$mg))
hist(sqrt(ice.off$mg))

ggplot(ice.off, aes(log(mg), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

#na vs cbv -  na deacrease
hist(ice.off$na)
hist(log(ice.off$na))
hist(sqrt(ice.off$na))

ggplot(ice.off, aes(na, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

#k vs cbv -  k deacrease-- eek 
hist(ice.off$k)
hist(log(ice.off$k))
hist(sqrt(ice.off$k))

ggplot(ice.off, aes(sqrt(k), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

#fe vs cbv -  fe deacrease 
hist(ice.off$fe)
hist(log(ice.off$fe))
hist(sqrt(ice.off$fe))

ggplot(ice.off, aes(sqrt(fe), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

#mn vs cbv -  no
hist(ice.off$mn)
hist(log(ice.off$mn))
hist(sqrt(ice.off$mn))

ggplot(ice.off, aes(sqrt(mn), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

#chlor.surf vs cbv -  no
hist(ice.off$chlor.surf)
hist(log(ice.off$chlor.surf))
hist(sqrt(ice.off$chlor.surf))

ggplot(ice.off, aes(sqrt(chlor.surf), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

#light vs cbv -  sqrt- slight increase
hist(ice.off$light)
hist(log(ice.off$light))
hist(sqrt(ice.off$light))

ggplot(ice.off, aes(sqrt(light), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

#iceduration vs cbv - slight decrease
hist(ice.off$iceduration)
hist(log(ice.off$iceduration))
hist(sqrt(ice.off$iceduration))

ggplot(ice.off, aes(log(iceduration), log.cbv))+
  geom_point()+
  geom_smooth(method='lm')

#====

# begin model for ice-off

off.lm1 = lm(log.cbv ~ wtemp + log(chlor.surf)+ o2 + cond+ frlight+ log(phaeo)+ ph+ dic+ tic+ 
               doc+ log(no3no2) + log(nh4)+ sqrt(totpf)+ sqrt(totpuf)+
               sqrt(drsif)+ brsif+ so4+ log(ca)+ log(mg)+ na+ fe+ sqrt(light), data=ice.off)
summary(off.lm1) #essentially perfect fit: summary may be unreliable, adj r2 = 1
AIC(off.lm1) # -6.25           

#remove NAs from last round       
off.lm2 = lm(log.cbv ~ wtemp + log(chlor.surf)+ o2 + cond+ frlight+ log(phaeo)+ ph+ dic+ tic+ 
               doc+ log(no3no2) + log(nh4)+ sqrt(totpf)+ sqrt(totpuf), data=ice.off)
summary(off.lm2) #0.2362
AIC(off.lm2) #96.44296

#add back na from first round       
off.lm3 = lm(log.cbv ~ wtemp + log(chlor.surf)+ o2 + cond+ frlight+ log(phaeo)+ ph+ dic+ tic+ 
               doc+ log(no3no2) + log(nh4)+ sqrt(totpf)+ sqrt(totpuf)+ na, data=ice.off)
summary(off.lm3) #0.3233
AIC(off.lm3) #91.77461
#brsif==perfect fit,  s04-0.2094/98.30494, log(ca)-0.2352/96.90988, log(mg)- 0.2473/96.24414, 
#na-0.3233/91.77461, fe- 0.2074/98.4097, sqrt(light)- 0.164/97.12119

#remove log(nh4)        
off.lm4 = lm(log.cbv ~ wtemp + log(chlor.surf)+ o2 + cond+ frlight+ log(phaeo)+ ph+ dic+ tic+ 
               doc+ log(no3no2) + sqrt(totpf)+ sqrt(totpuf)+ na, data=ice.off)
summary(off.lm4) #0.3483
AIC(off.lm4) #89.7752

#remove log(no3no2)        
off.lm5 = lm(log.cbv ~ wtemp + log(chlor.surf)+ o2 + cond+ frlight+ log(phaeo)+ ph+ dic+ tic+ 
               doc + sqrt(totpf)+ sqrt(totpuf)+ na, data=ice.off)
summary(off.lm5) #0.3715
AIC(off.lm5) #87.78027

#remove doc        
off.lm6 = lm(log.cbv ~ wtemp + log(chlor.surf)+ o2 + cond+ frlight+ log(phaeo)+ ph+ dic+ tic+ 
            sqrt(totpf)+ sqrt(totpuf)+ na, data=ice.off)
summary(off.lm6) #0.3926
AIC(off.lm6) #85.82053
#brsif==perfect fit,  s04-perfect fit, log(ca)-0.3581/89.13851, log(mg)- 0.3482/89.78018, 
# fe- 0.3666/88.57769, sqrt(light)- 0.3229/88.319

#remove frlight        
off.lm6 = lm(log.cbv ~ wtemp + log(chlor.surf)+ o2 + cond+ log(phaeo)+ ph+ dic+ tic+ 
               sqrt(totpf)+ sqrt(totpuf)+ na, data=ice.off)
summary(off.lm6) #0.4144
AIC(off.lm6) #85.08451
#brsif==0.9475/-1.654115,  so4-0.4078/86.16218, log(ca)-0.4007/86.66959, log(mg)- 0.395/87.07934, 
# fe- 0.4067/86.23551, sqrt(light)- 0.3695/84.54554

#add brsif back in         
off.lm7 = lm(log.cbv ~ wtemp + log(chlor.surf)+ o2 + cond+ log(phaeo)+ ph+ dic+ tic+ 
               sqrt(totpf)+ sqrt(totpuf)+ na+ brsif, data=ice.off)
summary(off.lm7) #0.9475
AIC(off.lm7) #-1.654115

#remove sqrt(totpuf)          
off.lm8 = lm(log.cbv ~ wtemp + log(chlor.surf)+ o2 + cond+ log(phaeo)+ ph+ dic+ tic+ 
               sqrt(totpf)+ na+ brsif, data=ice.off)
summary(off.lm8) #0.9549
AIC(off.lm8) #-3.62354

#remove sqrt(totpf)
off.lm9 = lm(log.cbv ~ wtemp + log(chlor.surf)+ o2 + cond+ log(phaeo)+ ph+ dic+ tic+ 
              na+ brsif, data=ice.off)
summary(off.lm9) #0.9339
AIC(off.lm9) #4.203558

plot(off.lm9)

#remove sqrt(totpuf)          
#off.lm10 = lm(log.cbv ~ wtemp + log(chlor.surf)+ o2 + cond+ log(phaeo)+ ph+ dic+ tic+ 
#               sqrt(totpf)+ na, data=ice.off)
#summary(off.lm10) #0.428
#AIC(off.lm10) #83.44044

#remove o2        
#off.lm11 = lm(log.cbv ~ wtemp + log(chlor.surf)+ cond+ log(phaeo)+ ph+ dic+ tic+ 
#                sqrt(totpf)+ na, data=ice.off)
#summary(off.lm11) #0.4324
#AIC(off.lm11) #82.43368

#remove totpf        
#off.lm12 = lm(log.cbv ~ wtemp + log(chlor.surf)+ cond+ log(phaeo)+ ph+ dic+ tic+ 
#                 na, data=ice.off)
#summary(off.lm12) # 0.421
#AIC(off.lm12) #82.57316

#remove log(phaeo)        
#off.lm12 = lm(log.cbv ~ wtemp + log(chlor.surf)+ cond+ ph+ dic+ tic+ 
#                na, data=ice.off)
#summary(off.lm12) # 0.421
#AIC(off.lm12) #82.57316

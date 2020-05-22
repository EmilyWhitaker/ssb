## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz 
library(lubridate) # dealing with dates

#====
totals = read.csv('data/abiotic_total_biovolume.csv',stringsAsFactors = F)
totals$sampledate = ymd(totals$sampledate)
totals$log.cbv = log(totals$CellBioVol)
ice.off = subset(totals, ice.pres == 0)
write.csv(ice.off,'data/iceoff.csv')

hist(ice.off$CellBioVol)
hist(log(ice.off$CellBioVol))
shapiro.test(log(totals$CellBioVol))

ggplot(ice.off, aes(year, iceduration))+
  geom_point()+
  geom_smooth(method='lm')
#==== 
ggplot(ice.off, aes(doc, log.cbv))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~ice.pres, scales="free")

#====

lm1 = lm(log.cbv ~ wtemp + o2 + o2sat+ cond+ frlight+ alk+ doc+ dic+ nh4+ ph + 
           light + chlor.int+ iceduration, data=ice.off)
summary(lm1) #adj r2:  0.19  
AIC(lm1) # 96.47055, chlor.int is sig

#remove doc
lm2 = lm(log.cbv ~ wtemp + o2 + o2sat+ cond+ frlight+ alk+ dic+ nh4+ ph + 
           light + chlor.int+ iceduration, data=ice.off)
summary(lm2) #adj r2:  0.2189  
AIC(lm2) # 94.47386, chlor.int is sig

#remove ice duration 
lm3 = lm(log.cbv ~ wtemp + o2 + o2sat+ cond+ frlight+ alk+ dic+ nh4+ ph + 
           light + chlor.int, data=ice.off)
summary(lm3) #adj r2:  0.2458  
AIC(lm3) # 92.47529, chlor.int is sig

#remove ph 
lm4 = lm(log.cbv ~ wtemp + o2 + o2sat+ cond+ frlight+ alk+ dic+ nh4 + 
           light + chlor.int, data=ice.off)
summary(lm4) #adj r2:  0.2707  
AIC(lm4) # 90.48912, chlor.int is sig

#remove alk
lm5 = lm(log.cbv ~ wtemp + o2 + o2sat+ cond+ frlight + dic+ nh4 + 
           light + chlor.int, data=ice.off)
summary(lm5) #adj r2:  0.2878  
AIC(lm5) # 88.86053, chlor.int is sig

#remove frlight
lm6 = lm(log.cbv ~ wtemp + o2 + o2sat+ cond + dic+ nh4 + 
           light + chlor.int, data=ice.off)
summary(lm6) #adj r2:  0.2952  
AIC(lm6) # 87.7345, chlor.int is sig

#remove cond-- 
lm7 = lm(log.cbv ~ wtemp + o2 + o2sat + dic+ nh4 + 
           light + chlor.int, data=ice.off)
summary(lm7) #adj r2:  0.2356  
AIC(lm7) # 115.3041, chlor.int is sig

#add cond back remove light
lm8 = lm(log.cbv ~ wtemp + o2 + o2sat + dic+ nh4 + cond + chlor.int, data=ice.off)
summary(lm8) #adj r2:  0.2974  
AIC(lm8) # [1] 91.32507, chlor.int is sig, wtemp, 02, 02sat sig

lm6 = lm(log.cbv ~ wtemp + o2 + o2sat+ cond + dic+ nh4 + 
           light + chlor.int, data=ice.off)
summary(lm6) #adj r2:  0.2952  
AIC(lm6) # 87.7345, chlor.int is sig



# remove dic
lm9 = lm(log.cbv ~ wtemp + o2 + o2sat + nh4 + cond, data=ice.off)
summary(lm9) #adj r2:  0.2974  
AIC(lm9) # [1] 91.32507, chlor.int is sig, wtemp, 02, 02sat sig

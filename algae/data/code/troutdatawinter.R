#Trout Bog snow and ice over time 

## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin)

############

TB_snowice= read.csv('data/Trout data/snowiceTB.csv', stringsAsFactors = F)
TB_snowice$sampledate = mdy(TB_snowice$sampledate)

TB_bulkdata= read.csv('data/emilyTBslice.csv', stringsAsFactors = F)
TB_bulkdata$sampledate = mdy(TB_bulkdata$sampledate)
TB_bulkdata %<>% rename(chlor.int= chlor)

TB_chloro= read.csv('data/troutchloro.csv', stringsAsFactors = F)
TB_chloro %<>% subset(depth ==0)
write.csv(TB_chloro, 'data/TB_chloro.csv', row.names = F)
TB_chloro.surf= read.csv('data/TB_chloro.surf.csv', stringsAsFactors = F)
TB_chloro.surf$sampledate = mdy(TB_chloro.surf$sampledate)
#fuzzy join snow ice and bulk

TB_snowice %<>% select(sampledate, avsnow, totice, whiteice, blueice, iceduration, season) %>%
  mutate(bv.datePlus1 = sampledate + 1) %>% mutate(bv.dateMinus1 = sampledate - 1)

TB_IceBD <-fuzzy_left_join(TB_bulkdata, TB_snowice, by = c("sampledate" = "bv.datePlus1", "sampledate" = "bv.dateMinus1"),
                                 match_fun = list(`<=`, `>=`))


TB_IceBD$avsnow[is.na(TB_IceBD$avsnow)]=0
TB_IceBD$totice[is.na(TB_IceBD$totice)]=0
TB_IceBD$blueice[is.na(TB_IceBD$blueice)]=0
TB_IceBD$whiteice[is.na(TB_IceBD$whiteice)]=0

TB_chloro.surf %<>% select(sampledate, chlor.surf) %>%
  mutate(bv.datePlus1 = sampledate + 1) %>% mutate(bv.dateMinus1 = sampledate - 1)

TB_IceBD <-fuzzy_left_join(TB_IceBD, TB_chloro.surf, by = c("sampledate.x" = "bv.datePlus1", "sampledate.x" = "bv.dateMinus1"),
                           match_fun = list(`<=`, `>=`))

write.csv(TB_IceBD, 'data/TB_IceBD.csv', row.names = F)


TB_Clean= read.csv('data/TB_IceBD.clean.csv', stringsAsFactors = F)






###################


TB_snowice= read.csv('data/Trout data/snowiceTB.csv', stringsAsFactors = F)
TB_snowice$sampledate = mdy(TB_snowice$sampledate)
TB_snowice.long = pivot_longer(TB_snowice, cols=c("whiteice","blueice"), names_to="variable", values_to = "value")

#data.long2 = pivot_longer(data, cols=c("whiteice","blueice"), names_to="variable", values_to = "value")



ggplot(TB_snowice.long, aes(sampledate, value,color=variable))+
  ylab('cm')+
  geom_point()+
  geom_smooth(aes(group=variable))+
  geom_bar(aes(group=variable))+
  geom_smooth(aes(group=variable))

a<-ggplot(TB_snowice.long, aes(fill=variable, y=value, x=sampledate))+
  xlab("Sample Date")+
  ylab("Ice Thickness (cm)")+
  labs(fill='Ice Composition')+
  theme_classic()+
#  theme(axis.text.x=element_text(size=rel(1.5)))+
  geom_bar(position = "stack", stat= "identity", width = 80)
a
ggsave("tbicecomp.png", plot=a ,width = 6, height = 3, units = 'in')

#How are you exporting your figures? Leave the text size as default (aka, don't include that line), and make sure you're 
#exporting your figures to the appropriate width/height that your want them. Like ggsave(figure.png, width = 6, height = 3, units = 'in')


ggplot(TB_snowice.long, aes(sampledate, value,color=variable))+
  ylab('cm')+
  geom_point()+
  geom_smooth(aes(group=variable))+
  geom_bar(aes(group=variable))+
  geom_smooth(aes(group=variable))

b<-ggplot(TB_snowice.long, aes(y=avsnow, x=sampledate))+
  xlab("Sample Date")+
  ylab("Average Snow (cm)")+
#  theme_update(text = element_text(size=600))+
  theme_classic()+
  geom_bar(stat= "identity", width= 100, fill= "grey")
b
ggsave("tbsnowcomp.png", plot=b ,width = 6, height = 3, units = 'in')

ggplot(TB_snowice.long, aes(sampledate, avsnow))+
  ylab('average snow fall (cm)')+
  xlab('sample date')+
  geom_point()+
  geom_smooth()

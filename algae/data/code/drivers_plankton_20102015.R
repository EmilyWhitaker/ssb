#2010-2015 North sampling (TB and SP data)

# plot with light, ice type, thickness, zoops, chl-a

## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin)

#########
#merge zoops with rest of the dataset
SPZoops=read.csv('data/SPZoops.csv', stringsAsFactors = F)
SPZoops$sample_date = ymd(SPZoops$sample_date)
SPZoops$year= year(SPZoops$sample_date)
SPZoops %<>% rename(sampledate = sample_date)

#write.csv(SP_data_season_total, 'data/SP_data_season_total.csv', row.names = F)
SPdataset = read.csv('data/joinedSPseasonFull_06172020.csv', stringsAsFactors = F)
SPdataset %<>% rename(sampledate.y = sampledate)
SPdataset %<>% rename(sampledate = sampledate.x)
SPdataset$sampledate = mdy(SPdataset$sampledate)

SPZoops %<>% select(sampledate, species_code, species_name, density, individuals_measured, avg_length) %>%
  mutate(bv.datePlus1 = sampledate + 1) %>% mutate(bv.dateMinus1 = sampledate - 1)


join_zoops_SP <-fuzzy_left_join(SPdataset, SPZoops, by = c("sampledate" = "bv.datePlus1", "sampledate" = "bv.dateMinus1"),
                                match_fun = list(`<=`, `>=`))
#clean dataset
#join_zoops_SP %<>% rename(sampledate.y = sampledate)
#join_zoops_SP %<>% rename(sampledate = sampledate.x)
join_zoops_SP %<>% rename(chlor.int = chlor.x)
join_zoops_SP %<>% rename(chlor.surf = chlor.y)
join_zoops_SP$avsnow[is.na(join_zoops_SP$avsnow)]=0
join_zoops_SP$totice[is.na(join_zoops_SP$totice)]=0
join_zoops_SP$blueice[is.na(join_zoops_SP$blueice)]=0
join_zoops_SP$whiteice[is.na(join_zoops_SP$whiteice)]=0

write.csv(join_zoops_SP, 'data/joinedAllVars07152020.csv', row.names = F)
###################

join_zoops_SP = read.csv('data/joinedAllVars07152020_clean.csv', stringsAsFactors = F)
join_zoops_SP$sampledate = mdy(join_zoops_SP$sampledate)
join_zoops_SP %<>% rename(year4 = year)
join_zoops_SP$month = month(join_zoops_SP$sampledate)
join_zoops_SP$year = year(join_zoops_SP$sampledate)

ggplot(join_zoops_SP, aes(month, density))+
  geom_point(aes(col=Season), size=2)+
  # geom_smooth()+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='month', y= 'density')+
  facet_wrap(~ month + species_code, scales = 'free')


a<-ggplot(join_zoops_SP, aes(month==1, density))+
  geom_point(aes(col=Season), size=2)+
  theme_classic()+
  facet_wrap(~species_code, scales = 'free')
a


dataset1015 <-subset(join_zoops_SP, year>2009)
####################
COPEPOD_NAUPLII <-subset(join_zoops_SP, species_code== 10000)
CYCLOPOID <-subset(join_zoops_SP, species_code== 20000)
DIACYCLOPS_THOMASI <-subset(join_zoops_SP, species_code== 20302)
CALANOID <-subset(join_zoops_SP, species_code== 30000)
LEPTODIAPTOMUS_MINUTUS <-subset(join_zoops_SP, species_code== 30402)
SKISTODIAPTOMUS_OREGONENSIS <-subset(join_zoops_SP, species_code== 30801)
DAPHNIA_MENDOTAE <-subset(join_zoops_SP, species_code== 51103)
DAPHNIA_LONGIREMIS <-subset(join_zoops_SP, species_code== 51104)
DAPHNIA_PULICARIA <-subset(join_zoops_SP, species_code== 51130)
HOLOPEDIUM <-subset(join_zoops_SP, species_code== 51800)
ASPLANCHNA <-subset(join_zoops_SP, species_code== 60300)
CONOCHILUS_UNICORNIS <-subset(join_zoops_SP, species_code== 60902)
FILINIA <-subset(join_zoops_SP, species_code== 61400)
KELLICOTTIA_BOSTONIENSIS <-subset(join_zoops_SP, species_code== 61701)
KELLICOTTIA_LONGISPINA <-subset(join_zoops_SP, species_code== 61702)
KERATELLA_COCHLEARIS <-subset(join_zoops_SP, species_code== 61801)
KERATELLA_HIEMALIS <-subset(join_zoops_SP, species_code== 61805)
KERATELLA_QUADRATA <-subset(join_zoops_SP, species_code== 61806)
LECANE <-subset(join_zoops_SP, species_code== 61900)
LECANE_INERMIS <-subset(join_zoops_SP, species_code== 61904)
POLYARTHRA_DOLICHOPTERA <-subset(join_zoops_SP, species_code== 63001)
POLYARTHRA_VULGARIS <-subset(join_zoops_SP, species_code== 63005)
ACANTHOCYCLOPS_VERNALIS <-subset(join_zoops_SP, species_code== 20101)
DIACYCLOPS <-subset(join_zoops_SP, species_code== 20300)
MESOCYCLOPS_EDAX <-subset(join_zoops_SP, species_code== 20601)
TROPOCYCLOPS_PRASINUS_MEXICANUS <-subset(join_zoops_SP, species_code== 20901)
EPISCHURA_LACUSTRIS <-subset(join_zoops_SP, species_code== 30201)
DIAPTOMID <-subset(join_zoops_SP, species_code== 30400)
LEPTODIAPTOMUS_MINUTUS <-subset(join_zoops_SP, species_code== 30402)
SKISTODIAPTOMUS_OREGONENSIS <-subset(join_zoops_SP, species_code== 30801)
CLADOCERAN <-subset(join_zoops_SP, species_code== 50000)
ALONA <-subset(join_zoops_SP, species_code== 50300)
BOSMINIDAE <-subset(join_zoops_SP, species_code== 50700)
CERIODAPHNIA <-subset(join_zoops_SP, species_code== 50900)
CHYDORUS <-subset(join_zoops_SP, species_code== 51000)
DAPHNIA <-subset(join_zoops_SP, species_code== 51100)
DAPHNIA_AMBIGUA <-subset(join_zoops_SP, species_code== 51101)
DAPHNIA_DUBIA <-subset(join_zoops_SP, species_code== 51102)
DAPHNIA_MENDOTAE <-subset(join_zoops_SP, species_code== 51103)
DAPHNIA_LONGIREMIS <-subset(join_zoops_SP, species_code== 51104)
DAPHNIA_PARVULA <-subset(join_zoops_SP, species_code== 51105)
DAPHNIA_RETROCURVA <-subset(join_zoops_SP, species_code== 51107)
DAPHNIA_DENTIFERA <-subset(join_zoops_SP, species_code== 51108)
DAPHNIA_PULICARIA <-subset(join_zoops_SP, species_code== 51130)
DIAPHANOSOMA <-subset(join_zoops_SP, species_code== 51200)
DIAPHANOSOMA_BIRGEI <-subset(join_zoops_SP, species_code== 51202)
HOLOPEDIUM <-subset(join_zoops_SP, species_code== 51800)
HOLOPEDIUM_GIBBERUM <-subset(join_zoops_SP, species_code== 51801)
ANURAEOPSIS <-subset(join_zoops_SP, species_code== 60100)
ASCOMORPHA <-subset(join_zoops_SP, species_code== 60200)
ASCOMORPHA_ECAUDIS <-subset(join_zoops_SP, species_code== 60201)
ASCOMORPHA_OVALIS <-subset(join_zoops_SP, species_code== 60202)
ASCOMORPHA_SALTANS <-subset(join_zoops_SP, species_code== 60203)
ASPLANCHNA <-subset(join_zoops_SP, species_code== 60300)
CEPHALODELLA <-subset(join_zoops_SP, species_code== 60500)
COLLOTHECA <-subset(join_zoops_SP, species_code== 60600)
COLLOTHECA_MUTABILIS <-subset(join_zoops_SP, species_code== 60601)
COLURELLA <-subset(join_zoops_SP, species_code== 60700)
CONOCHILOIDES <-subset(join_zoops_SP, species_code== 60800)
CONOCHILUS <-subset(join_zoops_SP, species_code== 60900)
CONOCHILUS_HIPPOCREPIS <-subset(join_zoops_SP, species_code== 60901)
CONOCHILUS_UNICORNIS <-subset(join_zoops_SP, species_code== 60902)
EUCHLANIS <-subset(join_zoops_SP, species_code== 61300)
FILINIA <-subset(join_zoops_SP, species_code== 61400)
FILINIA_TERMINALIS <-subset(join_zoops_SP, species_code== 61402)
GASTROPUS <-subset(join_zoops_SP, species_code== 61500)
GASTROPUS_HYPTOPUS <-subset(join_zoops_SP, species_code== 61501)
GASTROPUS_STYLIFER <-subset(join_zoops_SP, species_code== 61502)
KELLICOTTIA_BOSTONIENSIS <-subset(join_zoops_SP, species_code== 61701)
KELLICOTTIA_LONGISPINA <-subset(join_zoops_SP, species_code== 61702)
KERATELLA_COCHLEARIS <-subset(join_zoops_SP, species_code== 61801)
KERATELLA_COCHLEARIS_F_TECTA <-subset(join_zoops_SP, species_code== 61802)
KERATELLA_CRASSA <-subset(join_zoops_SP, species_code== 61803)
KERATELLA_EARLINAE <-subset(join_zoops_SP, species_code== 61804)
KERATELLA_HIEMALIS <-subset(join_zoops_SP, species_code== 61805)
KERATELLA_QUADRATA <-subset(join_zoops_SP, species_code== 61806)
KERATELLA_SERRULATA <-subset(join_zoops_SP, species_code== 61807)
KERATELLA_TAUROCEPHALA <-subset(join_zoops_SP, species_code== 61808)
KERATELLA_TESTUDO <-subset(join_zoops_SP, species_code== 61809)
KERATELLA_TICINENSIS <-subset(join_zoops_SP, species_code== 61810)
LECANE <-subset(join_zoops_SP, species_code== 61900)
LECANE_INERMIS <-subset(join_zoops_SP, species_code== 61904)
LECANE_MIRA <-subset(join_zoops_SP, species_code== 61911)
LECANE_TENUISETA <-subset(join_zoops_SP, species_code== 61918)
LEPADELLA <-subset(join_zoops_SP, species_code== 62000)
MONOSTYLA <-subset(join_zoops_SP, species_code== 62400)
MONOSTYLA_LUNARIS <-subset(join_zoops_SP, species_code== 62406)
MONOSTYLA_OBTUSA <-subset(join_zoops_SP, species_code== 62407)
NOTOMMATA <-subset(join_zoops_SP, species_code== 62700)
PLOESOMA <-subset(join_zoops_SP, species_code== 62900)
PLOESOMA_HUDSONI <-subset(join_zoops_SP, species_code== 62901)
PLOESOMA_LENTICULARE <-subset(join_zoops_SP, species_code== 62902)
POLYARTHRA <-subset(join_zoops_SP, species_code== 63000)
POLYARTHRA_DOLICHOPTERA <-subset(join_zoops_SP, species_code== 63001)
POLYARTHRA_EURYPTERA <-subset(join_zoops_SP, species_code== 63002)
POLYARTHRA_MAJOR <-subset(join_zoops_SP, species_code== 63003)
POLYARTHRA_REMATA <-subset(join_zoops_SP, species_code== 63004)
POLYARTHRA_VULGARIS <-subset(join_zoops_SP, species_code== 63005)
SYNCHAETA <-subset(join_zoops_SP, species_code== 63400)
TRICHOCERCA <-subset(join_zoops_SP, species_code== 63600)
TRICHOCERCA_BIROSTRIS <-subset(join_zoops_SP, species_code== 63602)
TRICHOCERCA_CYLINDRICA <-subset(join_zoops_SP, species_code== 63603)
TRICHOCERCA_MULTICRINIS <-subset(join_zoops_SP, species_code== 63612)
TRICHOTRIA <-subset(join_zoops_SP, species_code== 63700)
#############


SYNCHAETA_totals <-subset(SYNCHAETA, division== 'Total')
SYNCHAETA_Bacillariophyta <-subset(SYNCHAETA, division== 'Bacillariophyta')
SYNCHAETA_Chlorophyta <-subset(SYNCHAETA, division== 'Chlorophyta')
SYNCHAETA_Chrysophyta <-subset(SYNCHAETA, division== 'Chrysophyta')
SYNCHAETA_Cryptophyta <-subset(SYNCHAETA, division== 'Cryptophyta')
SYNCHAETA_Cyanophyta <-subset(SYNCHAETA, division== 'Cyanophyta')
SYNCHAETA_Haptophyta <-subset(SYNCHAETA, division== 'Haptophyta')
SYNCHAETA_Pyrrhophyta <-subset(SYNCHAETA, division== 'Pyrrhophyta')


syn<- ggplot(SYNCHAETA, aes(x=month, y=density, color=iceduration))+
  geom_point()+
  labs(title="Sparkling Lake SYNCHAETA Trends", x= "Month", y= "Density")+
  facet_wrap(~year)
syn
ggsave("syn.png") #need to work out details

syn.tot<- ggplot(SYNCHAETA_totals, aes(x=month, y=density, color=log(biovolume_conc)))+
  geom_point()+
  labs(title="Sparkling Lake SYNCHAETA Trends with Total Phytos Biovolumes", x= "Month", y= "Density")+
  facet_wrap(~year)
syn.tot
ggsave("syn.tot.png")                   

syn.Bac<- ggplot(SYNCHAETA_Bacillariophyta, aes(x=month, y=density, color=log(biovolume_conc)))+
  geom_point()+
  labs(title="Sparkling Lake SYNCHAETA Trends with Bacillariophyta Biovolumes", x= "Month", y= "Density")+
  facet_wrap(~year)
syn.Bac
ggsave("syn.Bac.png")   

syn.Bac1<- ggplot(SYNCHAETA_Bacillariophyta, aes(x=biovolume_conc, y=density, color=year))+
  geom_point()+
  labs(title="Sparkling Lake SYNCHAETA Trends with Bacillariophyta Biovolumes", x= "biovolume_conc", y= "Density")+
  facet_wrap(~month)
syn.Bac1
ggsave("syn.Bac1.png")   

syn.Chlo<- ggplot(SYNCHAETA_Chlorophyta, aes(x=month, y=density, color=log(biovolume_conc)))+
  geom_point()+
  labs(title="Sparkling Lake SYNCHAETA Trends with Chlorophyta Biovolumes", x= "Month", y= "Density")+
  facet_wrap(~year)
syn.Chlo
ggsave("syn.Chlo.png")  

syn.Chry<- ggplot(SYNCHAETA_Chrysophyta, aes(x=month, y=density, color=log(biovolume_conc)))+
  geom_point()+
  labs(title="Sparkling Lake SYNCHAETA Trends with Chrysophyta Biovolumes", x= "Month", y= "Density")+
  facet_wrap(~year)
syn.Chry
ggsave("syn.Chry.png") 

syn.Cry<- ggplot(SYNCHAETA_Cryptophyta, aes(x=month, y=density, color=log(biovolume_conc)))+
  geom_point()+
  labs(title="Sparkling Lake SYNCHAETA Trends with Cryptophyta Biovolumes", x= "Month", y= "Density")+
  facet_wrap(~year)
syn.Cry
ggsave("syn.Cry.png") 

syn.Cyan<- ggplot(SYNCHAETA_Cyanophyta, aes(x=month, y=density, color=log(biovolume_conc)))+
  geom_point()+
  labs(title="Sparkling Lake SYNCHAETA Trends with Cyanophyta Biovolumes", x= "Month", y= "Density")+
  facet_wrap(~year)
syn.Cyan
ggsave("syn.Cyan.png")

syn.Hapt<- ggplot(SYNCHAETA_Haptophyta, aes(x=month, y=density, color=log(biovolume_conc)))+
  geom_point()+
  labs(title="Sparkling Lake SYNCHAETA Trends with Haptophyta Biovolumes", x= "Month", y= "Density")+
  facet_wrap(~year)
syn.Hapt
ggsave("syn.Hapt.png")

syn.Pyrr<- ggplot(SYNCHAETA_Pyrrhophyta, aes(x=month, y=density, color=log(biovolume_conc)))+
  geom_point()+
  labs(title="Sparkling Lake SYNCHAETA Trends with Pyrrhophyta Biovolumes", x= "Month", y= "Density")+
  facet_wrap(~year)
syn.Pyrr
ggsave("syn.Pyrr.png")

##########



b<-ggplot(dataset1015, aes(dataset1015$month, dataset1015$density))+
  geom_point(aes(col=Season), size=2)+
  theme_classic()+
  facet_wrap(~ month + species_code, scales = 'free')
b




##########
#frlight
SPdataset.dates = read.csv('data/cleanSPdataset_limiteddates.csv', stringsAsFactors = F)
SPdataset.dates$sampledate = mdy(SPdataset.dates$sampledate)

#import ice thickness, light, zoops,



###################
#chloro-a
ChloroCheck = read.csv('data/SPChloro2010.Clean.withbv.csv', stringsAsFactors = F)

ChloroCheck$sampledate = mdy(ChloroCheck$sampledate)
ChloroCheck$month=month(ChloroCheck$sampledate)
ChloroCheck= ChloroCheck %>% select(year,	sampledate,	depth,	chlor, biovolume_conc, Season, month)		

#####

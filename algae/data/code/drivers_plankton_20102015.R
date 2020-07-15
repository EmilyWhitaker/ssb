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
join_zoops_SP = read.csv('data/joinedAllVars07152020_clean.csv', stringsAsFactors = F)
join_zoops_SP$sampledate = mdy(join_zoops_SP$sampledate)
join_zoops_SP %<>% rename(year = year4)
join_zoops_SP$month = month(join_zoops_SP$sampledate)

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
CYCLOPOID <-subset(join_zoops_SP, species_code== 51103)
CYCLOPOID <-subset(join_zoops_SP, species_code== 20000)
CYCLOPOID <-subset(join_zoops_SP, species_code== 20000)
CYCLOPOID <-subset(join_zoops_SP, species_code== 20000)
CYCLOPOID <-subset(join_zoops_SP, species_code== 20000)
CYCLOPOID <-subset(join_zoops_SP, species_code== 20000)
CYCLOPOID <-subset(join_zoops_SP, species_code== 20000)
CYCLOPOID <-subset(join_zoops_SP, species_code== 20000)
CYCLOPOID <-subset(join_zoops_SP, species_code== 20000)
CYCLOPOID <-subset(join_zoops_SP, species_code== 20000)

51103	DAPHNIA MENDOTAE

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

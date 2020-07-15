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

write.csv(join_zoops_SP, 'data/joinedAllVars07152020.csv', row.names = F)

#clean dataset
join_zoops_SP %<>% rename(sampledate.y = sampledate)
join_zoops_SP %<>% rename(sampledate = sampledate.x)
join_zoops_SP %<>% rename(chlor.int = chlor.x)
join_zoops_SP %<>% rename(chlor.surf = chlor.y)
join_zoops_SP$avsnow[is.na(join_zoops_SP$avsnow)]=0
join_zoops_SP$totice[is.na(join_zoops_SP$totice)]=0
join_zoops_SP$blueice[is.na(join_zoops_SP$blueice)]=0
join_zoops_SP$whiteice[is.na(join_zoops_SP$whiteice)]=0



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
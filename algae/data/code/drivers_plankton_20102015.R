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

##########
#frlight
SPdataset.dates = read.csv('data/cleanSPdataset_limiteddates.csv', stringsAsFactors = F)
SPdataset.dates$sampledate = mdy(SPdataset.dates$sampledate)

#import ice thickness, light, zoops,


#################
SPZoops=read.csv('data/SPZoops.csv', stringsAsFactors = F)
SPZoops$sample_date = ymd(SPZoops$sample_date)
SPZoops$year= year(SPZoops$sample_date)
SPZoops %<>% subset(year >= "2010")
SPZoops %<>% rename(sampledate = sample_date)


write.csv(SP_data_season_total, 'data/SP_data_season_total.csv', row.names = F)
SPdataset = read.csv('data/joinedSPseasonFull_06172020.csv', stringsAsFactors = F)


SPZoops %<>% select(sampledate, species_code, species_name, density, individuals_measured, avg_length) %>%
  mutate(bv.datePlus1 = sampledate + 1) %>% mutate(bv.dateMinus1 = sampledate - 1)

join_surfchlor_SP <-fuzzy_left_join(SPdataset, SPZoops, by = c("sampledate" = "bv.datePlus1", "sampledate" = "bv.dateMinus1"),
                                    match_fun = list(`<=`, `>=`))


completedataset=read.csv('data/joinedSPseasonFull_06172020', stringsAsFactors = F)


write.csv(join_surfchlor_SP, 'data/joinedSPseasonFull.csv', row.names = F)
class(join_surfchlor_SP$sampledate)
write.csv(SP_data_season_total, 'data/SP_data_season_total.csv', row.names = F)
joinedSPseasonFull_06172020








###################
#chloro-a
ChloroCheck = read.csv('data/SPChloro2010.Clean.withbv.csv', stringsAsFactors = F)

ChloroCheck$sampledate = mdy(ChloroCheck$sampledate)
ChloroCheck$month=month(ChloroCheck$sampledate)
ChloroCheck= ChloroCheck %>% select(year,	sampledate,	depth,	chlor, biovolume_conc, Season, month)		

#####
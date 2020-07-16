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


#######

#merging TB dataset-- eek 
TB_dataphyto = read.csv('data/TB_data_phytos.csv', stringsAsFactors = F)
TB_dataphyto$sampledate = ymd(TB_dataphyto$sampledate)
#need to make all of the absence data


TB_zoops= read.csv('data/TB_zoops.csv', stringsAsFactors = F)
TB_zoops$sampledate = mdy(TB_zoops$sampledate)
#need to make all of the absence data









#totalbv_TB = read.csv("data/TotalBVs.csv", stringsAsFactors = F)
#totalbv= read.csv("../TotalBVs.csv")
#totalbv$sampledate = ymd(totalbv$sampledate)
TB_data_season %<>% select(lakeid, sampledate, division, taxa_name, genus, biovolume_conc, Season) %>%
  mutate(bv.datePlus1 = sampledate + 1) %>% mutate(bv.dateMinus1 = sampledate - 1)

join_surfchlor <-fuzzy_left_join(join_ice, TB_data_season, by = c("sampledate" = "bv.datePlus1", "sampledate" = "bv.dateMinus1"),
                                 match_fun = list(`<=`, `>=`))

join_surfchlor %<>% rename(sampledate= sampledate.y)


write.csv(join_surfchlor, 'data/joinedTBseasonFull.csv', row.names = F)

class(join_surfchlor$sampledate)

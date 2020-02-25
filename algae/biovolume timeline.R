
#Legand of bv overtime  

#using Hildugs  Ice script 

library(tidyverse)
library(dplyr)
library(viridisLite)
library(lubridate)


############## Subset calibration data by depth for both epi and hypo ##########
#nutrients = read.csv('ntl1_v5.csv',stringsAsFactors = F) # Nutrients
#ions = read.csv('ntl2_v5.csv',stringsAsFactors = F) # Ions
#temp = read.csv('ntl29_v5.csv',stringsAsFactors = F) # Temp and Oxygen 
#chl = read.csv('ntl35_v2.csv',stringsAsFactors = F) # Chlorophyll

lakeAbr = 'AL'
colby = 'firsticeYDAY' # color options 'ice_duration','firsticeYDAY','lasticeYDAY' 

for (lakeAbr in c('ME','MO','WI','TR','SP','BM','AL','TB','CR','CB')) {
  
  lakestats = read_csv('../LTERlakes.csv') %>%
    dplyr::filter(LakeAbr == lakeAbr)
  
  lakename = lakestats$Characteristic
  minDepthEpi = 0
  maxDepthEpi = 2
  minDepthHypo = round(lakestats$`Maximum Depth (m)`) - 4
  maxDepthHypo = round(lakestats$`Maximum Depth (m)`) + 1
  # var = 'alk' #'wtemp' #o2sat' #wtemp,o2sat
  var1 = c('ph','alk','dic','tic','doc','toc','no3no2','no2','nh4','totnf','totnuf','totpf','totpuf','drsif','brsif','brsiuf','tpm','no3no2_sloh','nh4_sloh','kjdl_n_sloh','totpuf_sloh','drp_sloh','drsif_sloh')
  var2 = c('cl','so4','ca','mg','na','k','fe','mn','cond')
  var3 = c('o2sat','wtemp')
  var4 = c('chlor')
  convertYear <- function(date, target = 2000, adjust = F) {
    if (length(date) == 0 | is.na(date[1])){
      return(date)
      break
    }
    startYear = year(date)[1]
    adjustY = 0
    if (adjust == T) {
      if (month(date)[1] < 3) {adjustY = 1}
    }
    add = target-startYear + adjustY
    year(date) = year(date) + add
    return(date)
  }
  
  # Calculate duration of northern ice
#  iceDatesN = read_csv('ntl32_v5.csv') # Northern
   biovolume = read_csv('ntl33_v4.csv') %>% # Southern
    mutate(firstice = as.Date(ice_on,'%m/%d/%Y'), lastice = as.Date(ice_off,'%m/%d/%Y'), ice_duration = as.numeric(ice_duration), year4 = lead(as.numeric(year4))) %>%
    dplyr::select(year = year4,lakeid,ice_duration, firstice, lastice)
  
  iceDatesCombo = iceDatesN %>% 
    mutate(datefirsticeN = lag(datefirstice), firsticeN = lag(firstice)) %>%
    dplyr::mutate(firstIceX = ifelse(firsticeN < 100,0-firsticeN,365 - firsticeN), duration = lastice + firstIceX) %>%
    dplyr::mutate(firstIceX = ifelse(firsticeN < 100,0-firsticeN,365 - firsticeN), duration = lastice + firstIceX) %>%
    dplyr::select(year, lakeid,ice_duration = duration, firstice = datefirsticeN, lastice = datelastice) %>%
    bind_rows(iceDatesS) %>%
    mutate(firsticeYDAY = ifelse(yday(firstice) <50, yday(firstice) + 365,yday(firstice)), lasticeYDAY = yday(lastice))
  
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('red3','gold','deepskyblue4'))
  plot(1:100,col = rbPal(100),pch=16)
  
  iceDates = iceDatesCombo %>% dplyr::filter(lakeid == lakeAbr, year >= 1980) %>%
    mutate(durationZ = scale(get(colby))[,1]) %>%
    mutate(cols = rbPal(100)[as.numeric(cut(durationZ,breaks = 100))])
  
  # mutate(ice_on = as.Date(ice_on,'%m/%d/%Y'), ice_off = as.Date(ice_off,'%m/%d/%Y')) %>%
  # mutate(year = year4 + 1)
  
  
   i = i+ 1
        }
      }
    }
    dev.off()
  }
}

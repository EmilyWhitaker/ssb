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

##======
#Armored Dinoflagellate during ice-on

ice.on = subset(totals_and_genus, ice.pres == 1)
gen.keep1=c("Armored Dinoflagellate")
genus.sub1 = subset(totals_and_genus, Genus %in% gen.keep1)
ND.ice.on = subset(genus.sub1, ice.pres == 1) #correct


#========
#Armored Dinoflagellate ice off

ice.off = subset(totals_and_genus, ice.pres == 0)
gen.keep1=c("Armored Dinoflagellate")
genus.sub1 = subset(totals_and_genus, Genus %in% gen.keep1)
ND.ice.off = subset(genus.sub1, ice.pres == 0) #correct





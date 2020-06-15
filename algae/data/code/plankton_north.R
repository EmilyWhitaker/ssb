#Trout Bog snow and ice over time 

## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin)

##########
data = read.csv('data/phytoplankton_north.csv', stringsAsFactors = F)
data %<>% select(lakeid, year4, sampledate, division, taxa_name,relative_total_biovolume, genus, cells_per_nu,
                 nu_per_ml,	cells_per_ml,	biovolume_conc,	biomass_conc)
data$sampledate = ymd(data$sampledate)

data %<>% rename('white ice' = whiteice)
data %<>% rename('blue ice' = blueice)
clean %<>% rename(sampledate.genus = sampledate)
clean %<>% rename(sampledate = sampledate.x)
clean %<>% rename(chlor.int = chlor.x)
clean %<>% rename(chlor.surf = chlor.y)

clean$sampledate = ymd(clean$sampledate)


#seperate into TB

TB_data = subset(data, lakeid == "TB")


#SP data

SP_data <- subset(data, lakeid== "SP")



totals = subset(data, Genus == "TotalBiovolume")
totals$chlor = abs(totals$chlor)

# pull out genus-specific biovolumes
# skip ahead to clean file
genus = subset(data, Genus != "TotalBiovolume")







#make Holly's graph


## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin)

#####=======

data = read.csv('data/FINALfulldatasetclean05202020.csv', stringsAsFactors = F)
data$sampledate = ymd(data$sampledate)


#full dataset

#no.gerna.total = data %>% select(data = sampledate:blueice,year:iceduration)
#write.csv(no.gerna.total, 'data/clean05212020Nogenera.csv', row.names = F)
#ngt= read.csv("data/clean05212020NogeneraWithLables.csv", stringsAsFactors = F)
#ngt$sampledate = mdy(ngt$sampledate.x)
#ngt$year= year(ngt$sampledate)


clean = read.csv('data/datasetcleannogenera.csv', stringsAsFactors = F)
clean %<>% rename(sampledate.genus = sampledate)
clean %<>% rename(sampledate = sampledate.x)
clean %<>% rename(chlor.int = chlor.x)
clean %<>% rename(chlor.surf = chlor.y)
clean$sampledate = ymd(clean$sampledate)


data.total.long = pivot_longer(clean, cols=c("wtemp","o2", "avsnow","totice","whiteice","blueice", "chlor.int",
                                          "frlight","chlor.surf"), names_to="variable", values_to = "value")

ggplot(data.total.long, aes(year, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')

# look at winter total biovolume
totals.long$month = month(totals.long$sampledate)

winter = subset(totals.long, month < 4)

#only pull out winter sampling
ggplot(winter, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')







# pull out total biovolumes
totals = subset(data, Genus == "TotalBiovolume")




# pull out genus-specific biovolumes
# skip ahead to clean file
genus = subset(data, Genus != "TotalBiovolume")





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

ggplot(data.total.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')

# look at winter total biovolume
ice = read.csv('data/iceduration.csv', stringsAsFactors = F)
ice %<>% rename(ice.on = datefirstice,
                ice.off = datelastice)

ice$ice.on = mdy(ice$ice.on)
ice$ice.off = mdy(ice$ice.off)
ice %<>% subset(lakeid == "SP") %>%
  select(year,ice.off, ice.on, iceduration)

clean$year= year(clean$sampledate)

clean.ice = full_join(clean, ice, by=c('year'))
clean.ice$avsnow[is.na(clean.ice$avsnow)] = 0
clean.ice$totice[is.na(clean.ice$totice)] = 0
clean.ice$whiteice[is.na(clean.ice$whiteice)] = 0
clean.ice$blueice[is.na(clean.ice$blueice)] = 0


winter.clean= clean.ice%<>% subset(clean.ice$totice>0)
write.csv(winter.clean, 'data/nogeneraWinter.csv', row.names = F)
summer.clean= clean.ice%<>% subset(clean.ice$totice==0)
write.csv(summer.clean, 'data/nogeneraSummer.csv', row.names = F)


#only pull out winter sampling
winter.data.total.long = pivot_longer(winter.clean, cols=c("wtemp","o2", "avsnow","totice","whiteice","blueice", "chlor.int",
                                             "frlight","chlor.surf"), names_to="variable", values_to = "value")

ggplot(winter.data.total.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')



#pull out summer data
summer.data.total.long = pivot_longer(summer.clean, cols=c("wtemp","o2","avsnow","totice","whiteice","blueice", "chlor.int",
                                                           "frlight","chlor.surf"), names_to="variable", values_to = "value")

ggplot(summer.data.total.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  facet_wrap(~variable, scales='free')


#light graphs

ggplot(winter.clean, aes(frlight, totice))+
  geom_point(aes(group=frlight))+
  geom_point(size=2)+
  #scale_color_viridis_c(option="plasma")+
  geom_smooth(se=T)

ggplot(winter.clean, aes(frlight, blueice))+
  #  geom_point(aes(group=frlight))+
  geom_point(size=2)+
  #scale_color_viridis_c(option="plasma")+
  geom_smooth(se=T)

ggplot(winter.clean,                # plot data from data set plodata  
       aes(x= blueice,             # define x-axis
           y= frlight)) +  # define y-axis
  geom_point()  

ggplot(winter.clean,                # plot data from data set plodata  
       aes(x= whiteice,             # define x-axis
           y= frlight)) +  # define y-axis
  geom_point()  


ggplot(winter.clean,                # plot data from data set plodata  
       aes(x= avsnow,             # define x-axis
           y= frlight)) +  # define y-axis
  geom_point()  


# pull out total biovolumes
totals = subset(data, Genus == "TotalBiovolume")




# pull out genus-specific biovolumes
# skip ahead to clean file
genus = subset(data, Genus != "TotalBiovolume")


######
#Sliding windows
data
time- sample date, 
#slide_period()
#slide_index()



















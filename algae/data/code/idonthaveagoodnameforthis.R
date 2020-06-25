## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin); library(zoo)
library(evobiR)
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
clean$month=month(clean$sampledate)


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
install.packages("evobiR")

library(devtools)
install_github("coleoguy/evobir", build_vignettes = TRUE)

install.packages("evobiR", repo = 'https://mac.R-project.org')

sw1= subset(data, Genus == "TotalBiovolume")
sw1 %<>% select(sampledate,CellBioVol, chlor.int)

rollapply()


rollapply(sw1, 2*3-1, function(mean) max(rollapply(mean, 3, mean, na.rm = TRUE)), partial = TRUE)

SlidingWindow(mean(sw1), sw1, sampledate(2))


#slide_period()
#slide_index()

###############

SPChloroBVSeasons = read.csv('data/SPChloroalldateswithbvclean.csv', stringsAsFactors = F)
SPChloroBVSeasons$sampledate = mdy(SPChloroBVSeasons$sampledate)
SPChloroBVSeasons$month=month(SPChloroBVSeasons$sampledate)



Remove NAs from month, season, chloro

d <- ggplot(SPChloroBVSeasons, aes(chlor, depth))+
  geom_point(aes(col=Season), size=2)+
  #geom_smooth(method = 'lm')+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='Chlorophyll', y= 'Depth')+
  facet_wrap('month', scales = 'free')+
  labs(title="Sparkling Lake 1981-2009 Chlorophyll and Depth Relationship ")+
  xlim(0, NA)+
  ylim(18,0)
d


e <- ggplot(SPChloroBVSeasons, aes(chlor, CellBioVol))+
  geom_point(aes(col=depth))+
  #geom_smooth(method = 'lm')+
  #geom_line()+  
  theme_classic()+
  labs(x='Chlorophyll', y= 'Biovolume')+
  xlim(1,11)
e+ scale_color_gradientn(colours = rainbow(10))+ facet_grid(SPChloroBVSeasons$depth~ SPChloroBVSeasons$month, scales = 'free')


+ scale_color_gradientn(colours = rainbow(10))












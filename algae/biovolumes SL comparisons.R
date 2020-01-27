library(lubridate)
library(ggplot2)
library(ggridges)
library(tidyverse)

### bring in data

chems = read_csv('cheminfo.csv') %>%
  filter(lakeid == 'SP') 

chlor = read_csv('chloros.csv') %>%
  filter(lakeid == 'SP') %>%
  filter(depth == '0.00')

#switch date formatting 

par = read_csv('pardatasl.csv')

biovols = read_csv('biovolume1.csv') %>%
  filter(Genus == 'TotalBiovolume')

## create data files and clean

#par and totalbiovolume 
parbv= biovols %>%
  full_join(par, by = c('sampledate'))
write.csv(parbv, "parandbiovolume.csv")

cleanPARbv = read_csv('cleanparandbiovolume.csv')

#totalbios and chloros --- come back to need depth integration 
chlorosbv= biovols %>%
  full_join(chlor, by = c('sampledate'))
write.csv(chlorosbv, "chloroandbiovolume.csv")
   
#totalbios and chems 
  
  
#graphhh!!!
  
#par and totalbiovolume (year, par, biovolume, missing par data)
a<- ggplot(cleanPARbv, aes_(x=cleanPARbv$CellBioVol, y=cleanPARbv$extcoef))+ geom_point()+
  facet_wrap(cleanPARbv$year4)+
  labs(x = "Biovolume",
       y = "PAR")+
  theme_bw()
a



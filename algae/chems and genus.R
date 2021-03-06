library(lubridate)
library(ggplot2)
library(tidyverse)
library(readr)
dataset <- read_csv("sparkling_icesnowo2par.csv", 
                    col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))
View(dataset)

icey <- read_csv("iceduraation.csv") %>%
  filter(lakeid== 'SP')

chems = read_csv('cheminfo.csv') %>%
  filter(lakeid == 'SP') 


o2profile<- read_csv("sparkling_icesnowo2par.csv", 
  col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))%>% 
  select(sampledate, depth, o2) 

Limnothrix <- read_csv("biovolume1.csv", col_types = cols(sampledate = col_date(format = "%m/%d/%Y"))) %>%
  filter(Genus == 'Limnothrix')

#chlor = read_csv('chloros.csv') %>%
#  filter(lakeid == 'SP') %>%
#  filter(depth == '0.00')

totalchems = chems %>%
  right_join(o2profile, by= c('sampledate', 'depth'))
write.csv(totalchems, "o2chems.csv")


totalchems2 = read_csv('o2chems2.csv',
  col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))


zeroschem02 <- mutate(totalchems2 %>%
                        filter(depth == '0'))

### Limnothrix######
Limnothrix <- read_csv("biovolume1.csv", col_types = cols(sampledate = col_date(format = "%m/%d/%Y"))) %>%
  filter(Genus == 'Limnothrix')
Limnothrixatzero<- Limnothrix %>%
  right_join(zeroschem02, by= c('sampledate'))
write.csv(Limnothrixatzero, "Limnothrixatzero.csv")

Limnothrixatzero2 <- read_csv('Limnothrixatzero2.csv',
                              col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))

#####Microcystis ####
Microcystis <- read_csv("biovolume1.csv", col_types = cols(sampledate = col_date(format = "%m/%d/%Y"))) %>%
  filter(Genus == 'Microcystis')
Microcystisatzero<- Microcystis %>%
  right_join(zeroschem02, by= c('sampledate'))
write.csv(Microcystisatzero, "Microcystisatzero.csv")

##### Peanut#####
Peanut <- read_csv("biovolume1.csv", col_types = cols(sampledate = col_date(format = "%m/%d/%Y"))) %>%
  filter(Genus == 'Peanut')
Peanutatzero1<- Peanut %>%
  right_join(zeroschem02, by= c('sampledate'))
write.csv(Peanutatzero1, "Peanutatzero1.csv")
Peanutatzero2 <- read_csv('Peanutatzero1.csv',
                               col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))

### Cyclotella###

Cyclotella <- read_csv("biovolume1.csv", col_types = cols(sampledate = col_date(format = "%m/%d/%Y"))) %>%
  filter(Genus == 'Cyclotella')
Cyclotellaatzero<- Cyclotella %>%
  right_join(zeroschem02, by= c('sampledate'))
write.csv(Cyclotellaatzero, "Cyclotellaatzero.csv")

#####Naked Dinoflagellate #####

NakedDino <- read_csv("biovolume1.csv", col_types = cols(sampledate = col_date(format = "%m/%d/%Y"))) %>%
  filter(Genus == 'Naked Dinoflagellate')
NakedDinoatzero<- NakedDino %>%
  right_join(zeroschem02, by= c('sampledate'))
write.csv(NakedDinoatzero, "NakedDinoatzero.csv")

##Cryptomonas####
Cryptomonas <- read_csv("biovolume1.csv", col_types = cols(sampledate = col_date(format = "%m/%d/%Y"))) %>%
  filter(Genus == 'Cryptomonas')
Cryptomonasatzero<- Cryptomonas %>%
  right_join(zeroschem02, by= c('sampledate'))
write.csv(Cryptomonasatzero, "Cryptomonasatzero.csv")

###Armored Dinoflagellate###
Adino <- read_csv("biovolume1.csv", col_types = cols(sampledate = col_date(format = "%m/%d/%Y"))) %>%
  filter(Genus == 'Armored Dinoflagellate')
Adinoatzero<- Adino %>%
  right_join(zeroschem02, by= c('sampledate'))
write.csv(Adinoatzero, "Adinoatzero.csv")


### Dinobryon ##### 

Dinobryon <- read_csv("biovolume1.csv", col_types = cols(sampledate = col_date(format = "%m/%d/%Y"))) %>%
  filter(Genus == 'Dinobryon')
Dinobryonatzero<- Dinobryon %>%
  right_join(zeroschem02, by= c('sampledate'))
write.csv(Dinobryonatzero, "Dinobryonatzero.csv")

### Cf. Komvophoron / Trichormus ####
CfKT <- read_csv("biovolume1.csv", col_types = cols(sampledate = col_date(format = "%m/%d/%Y"))) %>%
  filter(Genus == 'Cf. Komvophoron / Trichormus')
CfKTatzero<- CfKT %>%
  right_join(zeroschem02, by= c('sampledate'))
write.csv(CfKTatzero, "CfKTatzero.csv")

#### Small cocconeis ####
smco <- read_csv("biovolume1.csv", col_types = cols(sampledate = col_date(format = "%m/%d/%Y"))) %>%
  filter(Genus == 'Small cocconeis')
smcoatzero<- smco %>%
  right_join(zeroschem02, by= c('sampledate'))
write.csv(smcoatzero, "smcoatzero.csv")


#### this graphing scheme works####

lzO<- ggplot(Limnothrixatzero2, aes(x= CellBioVol, y=o2))+geom_line()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "o2")+
  theme_bw()
lzO
ggsave(plot=lzO,filename='LimnothrixatzeroO2.png',height = 18, width =16, units = 'in')

  
lzph<- ggplot(Limnothrixatzero, aes(x= CellBioVol, y=ph))+geom_line()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "ph")+
  theme_bw()
lzph  
ggsave(plot=lzph,filename='Limnothrixatzeroph.png',height = 18, width =16, units = 'in')

#### Microcystis graphing###
Microcystisatzero2 <- read_csv('Microcystisatzero2.csv',
                              col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))
mzO<- ggplot(Microcystisatzero2, aes(x= CellBioVol, y=o2))+geom_line()+
  facet_wrap('group')+
  labs(x = "Biovolume Microcystis",
       y = "o2")+
  theme_bw()
mzO
ggsave(plot=mzO,filename='Microcystisatzero2.png',height = 18, width =16, units = 'in')

mzph<- ggplot(Microcystisatzero2, aes(x= CellBioVol, y=ph))+geom_line()+
  facet_wrap('group')+
  labs(x = "Biovolume Microcystis",
       y = "ph")+
  theme_bw()
mzph
ggsave(plot=mzph,filename='Microcystisatzerph.png',height = 18, width =16, units = 'in')

###Peanut###
pzO<- ggplot(Peanutatzero2, aes(x= CellBioVol, y=o2))+geom_line()+
  facet_wrap('group')+
  labs(x = "Biovolume Peanut",
       y = "o2")+
  theme_bw()
pzO
ggsave(plot=pzO,filename='Peanutatzero2.png',height = 18, width =16, units = 'in')

pzph<- ggplot(Peanutatzero2, aes(x= CellBioVol, y=ph))+geom_line()+
  facet_wrap('group')+
  labs(x = "Biovolume Peanut",
       y = "ph")+
  theme_bw()
pzph
ggsave(plot=pzph,filename='Peanutatzerph.png',height = 18, width =16, units = 'in')

####Cyclotella###
cyzO<- ggplot(Cyclotellaatzero, aes(x= CellBioVol, y=o2))+geom_line()
cyzO
### observed 1/14/97 through observed 11/16/00


###NakedDino####
ndzO<- ggplot(NakedDinoatzero, aes(x= CellBioVol, y=o2))+geom_line()+
  facet_wrap('group')+
  labs(x = "Biovolume naked dino",
       y = "o2")+
  theme_bw()
ndzO
ggsave(plot=ndzO,filename='ndatzero2.png',height = 18, width =16, units = 'in')


ndzph<- ggplot(NakedDinoatzero, aes(x= CellBioVol, y=ph))+geom_line()+
  facet_wrap('group')+
  labs(x = "Biovolume naked dino",
       y = "ph")+
  theme_bw()
ndzph
ggsave(plot=ndzph,filename='ndatzerph.png',height = 18, width =16, units = 'in')

#### Cryptomonas ##
cryzO<- ggplot(Cryptomonasatzero, aes(x= sampledate, y=CellBioVol))+geom_point() +
  facet_wrap('group')+
  labs(x = "Biovolume Cryptomonas",
       y = "o2")+
  theme_bw()
cryzO
ggsave(plot=cryzO,filename='Cryptomonasatzero2.png',height = 18, width =16, units = 'in')
     ### strange results?##
#####Armored Dinoflagellate###
Adinoatzero
adinozO<- ggplot(Adinoatzero, aes(x= CellBioVol, y=o2))+ geom_point() +
  facet_wrap('group')+
  labs(x = "Biovolume Adino",
       y = "o2")+
  theme_bw()
adinozO
ggsave(plot=adinozO,filename='Adinoatzeroo2.png',height = 18, width =16, units = 'in')

adzph<- ggplot(Adinoatzero, aes(x= CellBioVol, y=ph))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Armored dino",
       y = "ph")+
  theme_bw()
adzph
ggsave(plot=adzph,filename='Adinoatzerph.png',height = 18, width =16, units = 'in')

#####Dinobryon###
Dinobryonatzero

dinoozO<- ggplot(Dinobryonatzero, aes(x= CellBioVol, y=o2))+ geom_point() +
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "o2")+
  theme_bw()
dinoozO
ggsave(plot=dinoozO,filename='Dinobryonatzeroo2.png',height = 18, width =16, units = 'in')

dinoozph<- ggplot(Dinobryonatzero, aes(x= CellBioVol, y=ph))+ geom_point() +
  facet_wrap('group')+
  labs(x = "Biovolume Dinobryon",
       y = "ph")+
  theme_bw()
dinoozph
ggsave(plot=dinoozph,filename='Dinobryonatzeroph.png',height = 18, width =16, units = 'in')

### Cf. Komvophoron / Trichormus ####
CfKTozO<- ggplot(CfKTatzero, aes(x= CellBioVol, y=o2))+ geom_point() +
  facet_wrap('group')+
  labs(x = "Biovolume CfKT",
       y = "o2")+
  theme_bw()
CfKTozO
ggsave(plot=CfKTozO,filename='CfKTatzeroo2.png',height = 18, width =16, units = 'in')

CfKTozph<- ggplot(CfKTatzero, aes(x= CellBioVol, y=ph))+ geom_point() +
  facet_wrap('group')+
  labs(x = "Biovolume CfKT",
       y = "ph")+
  theme_bw()
CfKTozph
ggsave(plot=CfKTozph,filename='CfKTzeroph.png',height = 18, width =16, units = 'in')

####Small cocconeis ###
smcoozO<- ggplot(smcoatzero, aes(x= CellBioVol, y=o2))+ geom_point() +
  facet_wrap('group')+
  labs(x = "Biovolume Small cocconeis",
       y = "o2")+
  theme_bw()
smcoozO
ggsave(plot=smcoozO,filename='smcoatzeroo2.png',height = 18, width =16, units = 'in')

smcoozph<- ggplot(smcoatzero, aes(x= CellBioVol, y=ph))+ geom_point() +
  facet_wrap('group')+
  labs(x = "Biovolume Small cocconeis",
       y = "ph")+
  theme_bw()
smcoozph
ggsave(plot=smcoozph,filename='smcoatzeroph.png',height = 18, width =16, units = 'in')




gplot(cleanPARbv, aes_(x=cleanPARbv$CellBioVol, y=cleanPARbv$extcoef))+ geom_line()+
  facet_wrap(cleanPARbv$year4)+
  labs(x = "Biovolume",
       y = "PAR")+
  theme_bw()


a <-ggplot(zeroschem02, aes(x= ,y )+ geom_line()+
             labs(x = "Biovolume",
                  y = "PAR")+
             theme_bw()


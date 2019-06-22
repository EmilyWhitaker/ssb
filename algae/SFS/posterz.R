library(lubridate)
library(ggplot2)
library(ggridges)
library(tidyverse)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)



datasheet1 = chemsfull2 %>% 
  right_join(chloros,by = c('sampledate', 'depth', 'lakeid', 'year4'))
write.csv(datasheet1, "chloroandchems.csv")

###cell count vs year color in genus
theme_set(theme_classic())

# Histogram on a Categorical variable
g <- ggplot(data= phytos, aes(genus))
g + geom_density(aes(fill=factor(year)), alpha=0.8) + 
  labs(title="Density plot of genus", 
       subtitle="Genus by Years",
       x="Genus",
       fill="Year") 

f <- ggplot(data=phytos, aes(genus, phytos$month))
f + geom_bar(aes(fill=genus), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Manufacturer across Vehicle Classes") 

g <- ggplot(data=phytos, aes(genus, cellcount))
g + geom_violin() + 
  labs(title="Violin plot", 
       subtitle="City Mileage vs Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

ggplot(phytos, aes(x=genus,y = cellcount)) +geom_bar(stat = "identity")

p4 <- ggplot() + geom_bar(aes(y = cellcount, x = month, fill = genus), data = phytos, stat="identity")
p4
p4 + #scale_x_continuous(breaks=seq(1993,2013,1)) + 
  geom_point(dataset3,mapping = aes(x=month,y=chlor*500)) +
  geom_line(dataset3,mapping = aes(x=month,y=chlor*500))
  
  
  lines(x= p4, y= chlor)
p4

g <- ggplot(dataset3, aes(genus, cellcount))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

g <- ggplot(dataset3, aes(month, chlor))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

iceDatesN = read_csv('ntl32_v5.csv') # Northern

iceDatesCombo2 = iceDatesN %>% 
  mutate(datefirsticeN = lag(datefirstice), firsticeN = lag(firstice)) %>%
  dplyr::mutate(firstIceX = ifelse(firsticeN < 100,0-firsticeN,365 - firsticeN), duration = lastice + firstIceX) %>%
  dplyr::mutate(firstIceX = ifelse(firsticeN < 100,0-firsticeN,365 - firsticeN), duration = lastice + firstIceX) %>%
  dplyr::select(year, lakeid, ice_duration = duration, firstice = datefirsticeN, lastice = datelastice) %>%
  mutate(firsticeYDAY = ifelse(yday(firstice) <50, yday(firstice) + 365,yday(firstice)), lasticeYDAY = yday(lastice)) %>%
  mutate(iceon = as.Date('2000-01-01') + firsticeYDAY, iceoff = as.Date('2001-01-01') + lasticeYDAY)

lakestats = read_csv('LTERlakes.csv') 

dtf = iceDatesCombo2 %>% dplyr::select(lakeid,iceon,iceoff) %>%
  gather(group,date,iceon:iceoff) %>%
  dplyr::filter(lakeid != 'LR') %>%
  left_join(lakestats,by = c('lakeid'='LakeAbr'))


ggplotscatter(dataset3, x= cellcount, y= chloro)



plot(dataset3$chlor, dataset3$cellcount, main="Cell Count vs chlor", 
     xlab="chlor ", ylab="count ", pch=19)







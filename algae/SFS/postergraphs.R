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

datasheet3 = chlorochem %>%
  right_join(phytos, by = c('sampledate', 'year'))

###cell count vs year color in genus
theme_set(theme_classic())

fullsetgroup3$month2 <- month.abb[as.numeric(format(fullsetgroup3$sampledate, "%m"))]
fullsetgroup3$month2 <- factor(fullsetgroup3$month2, labels = month.abb)

#boxplot, chloro boxplot and genus box plot

genusbox<- ggplot(data=fullsetgroup3, aes(year, cellcount, fill=group))+
  geom_boxplot() + scale_y_log10() 
  
genusbox
  
typeof(dataaaa$month)


chlorobox <- ggplot(data=fullsetgroup3, aes(group(x=month(sampledate)), y=chlor))+
  geom_boxplot()
chlorobox

# Histogram on a Categorical variable
g <- ggplot(data= dataaaa, aes(genus))
g + geom_density(aes(fill=factor(year)), alpha=0.8) + 
  labs(title="Density plot of genus", 
       subtitle="Genus by Years",
       x="Genus",
       fill="Year") 

f <- ggplot(data=phytos, aes(genus, year))
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
  
ggplot(dataaaa, aes(x=genus,y = cellcount)) +geom_bar(stat = "identity") +
  facet_wrap(dataaaa$group)

p4 <- ggplot() + geom_bar(aes(y = cellcount, x = year, fill = genus), data = datasheet3, stat="identity") + facet_wrap(datasheet3$month)
p4
p4 <- p4 + scale_x_continuous(breaks=seq(1993,2013,1))
p4

##used?
p4 <- ggplot() + geom_bar(aes(y = cellcount, x = year, fill = genus), data = datasheet3, stat="identity") + facet_wrap(datasheet3$month)
p4

library(scales)
ggplot(datasheet3, aes(sampledate, cellcount, colour=genus)) + geom_line(size=.9) + 
  theme(legend.title = element_blank(), legend.position=c(.8,.2),axis.title.y=element_blank())


##used
ggplot(datasheet3) +
  geom_bar(aes(x = year, weight = cellcount, color=genus)) +
  geom_line(aes(x = year, y = chlor))


ggplot(datasheet3, aes(monthweek, weekdayf, fill = cellcount)) + 
  geom_tile(colour = "white") + 
  facet_grid(year~monthf) + 
  scale_fill_gradient(low="red", high="green") +
  labs(x="Week of Month",
       y="",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Yahoo Closing Price", 
       fill="Close")

#used
ggplot(fullsetgroup3, aes(x=chlor, y=cellcount, color= group)) + 
  geom_point()

##used 
xdensity <- ggplot(fullsetgroup3, aes(cellcount, fill=group)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#999999','#E69F00'))+
  scale_x_log10()
xdensity

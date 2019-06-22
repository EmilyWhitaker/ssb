library(lubridate)
library(ggplot2)
library(ggridges)
library(tidyverse)
library(gridExtra)

#boxplots
dataset$group = factor(dataset$group,levels = c('iceon','iceoff'),ordered=T)
genusbox<- ggplot(data=dataset, aes(dataset$year, cellcount))+
  geom_boxplot(aes(fill=group)) + scale_y_continuous(trans='log2') +
  scale_fill_manual(values = c('#99CCFF','#FF6666'),
                    name = "", labels = c("ice on",  "ice off")) +
  labs(y="Cell count", x = "Year") +
  theme_gray(base_size = 18) +
  theme(legend.position = c(.95,0.95)) 
genusbox
ggsave(plot = genusbox,filename = 'Figure_2.png',width = 14, height = 6, units = 'in')

chlorobox <- ggplot(fullsetgroup5, aes(fullsetgroup5$year, intchlor, fill=group))+
  geom_boxplot()
chlorobox
print(chlorobox + ggtitle("Yearly Chlorophyll Levels") +labs(y="Chlorophyll ug/l", x = "Year", fill = "Season"))

#used
ggplot(fullsetgroup3, aes(x=chlor, y=cellcount, color= group)) + 
  geom_point()

##used 
fullsetgroup3 = read_csv('fullsetgroup3.csv') %>% mutate(as.Date(sampledate,'%m/%d/%Y'))


pdensity <- ggplot(fullsetgroup3, aes(cellcount, fill=group)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#FF6666', '#99CCFF'))+
  scale_x_continuous(trans='log2') +
  labs(y="Density", x = "Cell Count", fill = "Season", tag='a)') +
  theme_gray(base_size = 18) +
  theme(legend.position = c(0.85,0.85)) 
pdensity
ggsave(plot = pdensity,filename = 'Figure_3.png',width = 6, height = 6, units = 'in')

cdensity <- ggplot(fullsetgroup3, aes(intchlor, fill=group)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#FF6666', '#99CCFF')) +
  labs(y="Density", x = "Chlorophyll (μg/l)", fill = "Season", tag = 'b)') +
  theme_gray(base_size = 18) +
  theme(legend.position = c(0.85,0.85))
cdensity
ggsave(plot = pdensity,filename = 'Figure_3b.png',width = 6, height = 6, units = 'in')

figure3 = grid.arrange(pdensity,cdensity,nrow = 1)
ggsave(plot = figure3,filename = 'Figure_3c.png',width = 12, height = 6, units = 'in')

### 
jb = read_csv('not2003.csv') %>% mutate(as.Date(sampledate,'%m/%d/%Y'))
jb$group = factor(jb$group,levels = c('iceon','iceoff'),ordered=T)

### genus work 
genusbox <- ggplot(jb, aes(genus, cellcount))+
  geom_boxplot(aes(fill= group))+
  scale_y_continuous(trans='log2')+
  labs(y="Cell count", x = "Genus") +
  theme_gray(base_size = 18) +
  scale_fill_manual(values = c('#99CCFF','#FF6666'),
                    name = "", labels = c("ice on",  "ice off")) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=0)) +
  facet_wrap(jb$year)
genusbox
ggsave(plot = genusbox,filename = 'Figure_4.png',width = 9, height = 6, units = 'in')

jb$genus = factor(jb$genus, levels = c('Other','Cryptomonad', 'Cyclotella','Fragilaria', 'Microcystis', 'Planktothrix'), ordered=T)
jb$group = factor(jb$group,levels = c('iceon','ice off'),ordered=T)

class(genus)

##chloro
genusbox<- ggplot(data=fullsetgroup5, aes(fullsetgroup5$year, chlor, fill= ))+
  geom_boxplot() + scale_y_continuous(trans='log2') 
genusbox

chlorobox <- ggplot(data=dataset, aes(dataset$year, chlor, fill=group))+
  geom_boxplot()
chlorobox
chlorobox <- ggplot(data=dataset, aes(dataset$year, dataset$`depth integrated`, fill=group))+
  geom_boxplot()
chlorobox
nData = chloro3 %>% group_by(year) %>% summarise(count = n())

groupped = read_csv('~/Documents/GitHub/Sparkling/algae/SFS/groupped.csv') %>% 
  mutate(sampledate = as.Date(sampledate,'%m/%d/%Y'))
# 
# p = function(v) {Reduce(f=paste0,x=v)}
# textnum = groupped %>% group_by(year,group) %>% tally() %>% 
#   ungroup() 
# textnum = textnum %>% dplyr::group_by(year) %>% 
#   summarise(test = p(as.character(n)))
# textnum$group = factor(textnum$group,levels = c('iceon surface','iceon depth','iceoff surface','iceoff depth'),ordered=T)

groupped$group = factor(groupped$group,levels = c('iceon surface','iceon depth','iceoff surface','iceoff depth'),ordered=T)
chlorobox <- ggplot(data=groupped, aes(x = as.character(year), y = `from group`))+
  geom_boxplot(aes(fill=group)) +
  scale_fill_manual(values = c('#3300CC', '#99CCFF','#CC0000','#FF6666'),
                    name = "", labels = c("Surface ice on", "Integrated ice on","Surface no-ice","Integrated no-ice" )) +
  labs(y="Chlorophyll (μg/l)", x = "Year") +
  theme_gray(base_size = 18) +
  theme(legend.position = c(0.93,0.94)) 
  chlorobox
ggsave(plot = chlorobox,filename = 'Figure_1.png',width = 14, height = 6, units = 'in')



#This is cool - and what would make it better is having a color scheme that 
#fits the categories a bit better. For instance, light blue (surface ice), 
#dark blue (depth ice), light red (surface no-ice), dark red (depth no-ice)… 
#or something like that

geom_text(data = chloro3, aes(x = year, y = 4, label = paste0('n = ',count))
print(chlorobox + ggtitle("Yearly Chlorophyll Levels") +labs(y="Chlorophyll ug/l", x = "Year", fill = "Season"))



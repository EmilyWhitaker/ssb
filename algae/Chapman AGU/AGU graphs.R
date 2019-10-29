library(lubridate)
library(ggplot2)
library(ggridges)
library(tidyverse)

biovolume = read_csv('boivolume1.csv') %>% mutate(as.Date(month,year))


boivolume1 = read_csv('boivolume1.csv') %>%
  col_types = cols(month = col_character(), 
                 year = col_character())

#data1 = read_csv('USE THIS SHEET FOR BIOVOLIE.csv')%>%mutate --- fill this out
biovolume$group = factor(boivolume1$group,levels = c('iceon','iceoff'),ordered=T)

bvbox<- ggplot(data=boivolume1, aes(year, Biovolume))+
  geom_boxplot(aes(fill=group)) + scale_y_continuous(trans='log2') +
  scale_fill_manual(values = c('#99CCFF','#FF6666'),
                    name = "", labels = c("ice on",  "ice off")) +
  labs(y="", x = "Year") +
  theme_bw(base_size = 18) +
  theme(legend.position = c(1,1)) 
bvbox

cellbox<- ggplot(data=boivolume1, aes(year, CellCount))+
  geom_boxplot(aes(fill=group)) + scale_y_continuous(trans='log2') +
  scale_fill_manual(values = c('#99CCFF','#FF6666'),
                    name = "", labels = c("ice on",  "ice off")) +
  labs(y="cell count / biovolume", x = "Year") +
  theme_bw(base_size = 18) +
  theme(legend.position = c(1.5,1.5)) 
cellbox
theme(legend.position = c(.95,0.95)) 
#merge bvbox and cellbox to an A/B plot

install.packages('cowplot')
library(cowplot)
legend <- get_legend(bvbox)
cellbox2 <- cellbox + theme(legend.position="none")

eeek <- plot_grid(cellbox, bvbox, labels = c('A', 'B'),  nrow=1)
eeek
ggsave(plot=eeek,filename='fig3.png',height = 10, width =25, units = 'in')

## emily mess with this 







ggplot(data = boivolume1, aes(x = year, y = Biovolume, fill = group)) + geom_bar(stat = "identity") + 
  facet_wrap(~ month)


test<- data.frame(boivolume1$Biovolume, boivolume1$CellCount, boivolume1$year)
ggplot(test, aes(x=year, y=value, fill=group)) +
  geom_bar(stat='identity', position='dodge')


monthplot<- ggplot(data=boivolume1, aes(boivolume1$month, Biovolume))+
  geom_boxplot(aes(fill=group)) + scale_y_continuous(trans='log2') +
  scale_fill_manual(values = c('#99CCFF', '#FF6666'),
                    name = "", labels = c("ice on","ice off")) +
  labs(y="biovolume", x = "month") +
  theme_gray(base_size = 18) +
  theme(legend.position = c(.95,0.95)) 
monthplot
#need to reorder 














#winter months with weather overlayed//biovolume

weather<- group_by(boivolume1$InchesSnow, boivolume1$whiteice, boivolume1$blackice, boivolume1$totalice)

weatherplots <- ggplot(data=boivolume1,aes(year, boivolume1$InchesSnow))+
                         geom_line()+
                         geom_point()+
  labs(y="inches of snow", x = "year") 
weatherplots













x1 <- boivolume1$year
df1 <- data.frame(x=x1,y= boivolume1$InchesSnow,type="Snow")
x2 <- boivolume1$year
df2 <- data.frame(x=x2,y= boivolume1$blackice,type="Black Ice")
x3 <- boivolume1$year
df3 <- data.frame(x=x3,y= boivolume1$whiteice,type="White Ice")
x4 <- boivolume1$year
df4 <- data.frame(x=x4,y= boivolume1$totalice,type="Total Ice")

df <- rbind(df1,df2,df3, df4)






weather_plots <- ggplot(df)+geom_point(aes(x,y,colour=type))
weather_plots
  #needs to be more temporial 
weatherandbio<- ggplot(data=boivolume1, aes(year, Biovolume))+
  geom_boxplot(aes(fill=group)) + scale_y_continuous(trans='log2') +
  scale_fill_manual(values = c('#99CCFF','#FF6666'),
                    name = "", labels = c("ice on",  "ice off")) +
  labs(y="biovolume", x = "Year") +
  theme_gray(base_size = 18) +
  theme(legend.position = c(.95,0.95)) 
weatherandbio







#biovolume over weather ----


  

gnpbv<-ggplot(data=boivolume1, aes(boivolume1$Genus, boivolume1$PerBioVol))+
  labs(y="biovolume", x = "genus") +
  theme_gray(base_size = 18) +
  theme(legend.position = c(.95,0.95)) 
gnpbv



p4 <- ggplot() + geom_bar(aes(y = boivolume1$PerBioVol, x = year, fill = Genus), data = boivolume1, stat="identity") 
p4














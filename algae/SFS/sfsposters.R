library(tidyverse)
library(lubridate)
library(ggplot2)
install.packages("plotly")
library(plotly)
library(dplyr)

dataset3 <- phytos %>%
  left_join(dataset,by = c('sampledate', 'year'))

write.csv(dataset3, "fullset.csv")


fullset <- dataset3 %>%
  full_join(dtf, by = c('sampledate'='date','lakeid'))

write.csv(fullset, "fullsetofinfo.csv")

#graph these thingsssssss

g <- ggplot(data= fullset, aes(genus))
g + geom_density(aes(fill=factor(year)), alpha=0.8) + 
  labs(title="Density plot of genus", 
       subtitle="Genus by Years",
       x="Genus",
       fill="Year") 



f <- ggplot(data=dataaaa, aes(genus, dataaaa$month))
f + geom_bar(aes(fill=genus), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Manufacturer across Vehicle Classes") 

#Create data
a= dataset3$chlor
b= dataset3$year
c= dataset3$genus
d= dataset3$cellcount

#Divide the screen in 2 columns and 2 lines
par(mfrow=c(2,2))

#Add a plot in each sub-screen !
plot( a,b , pch=20)
plot(c,d , pch=18)
hist(a, border=F , col=rgb(0.2,0.2,0.8,0.7) , main="")
boxplot(a , col="grey" , xlab="a")


sample=paste(rep("sample_",40) , seq(1,40) , sep="")
specie=c(rep("carot" , 10) , rep("cumcumber" , 10) , rep("wheat" , 10) , rep("Potatoe" , 10) )
gene1=c( seq(5,14)+rnorm(10 , 4 , 1) , seq(5,14)+rnorm(10 , 4 , 1) , seq(5,14)+rnorm(10 , 4 , 1) , seq(5,14)+rnorm(10 , 4 , 1) )
gene2=c( seq(5,14)+rnorm(10 , 4 , 1) , seq(5,14)+rnorm(10 , 2 , 0.2) , seq(5,14)+rnorm(10 , 4 , 4) , seq(5,14)+rnorm(10 , 4 , 3) )
data=data.frame(sample,specie,gene1,gene2)

# Make the graph
library(lattice)
xyplot(cellcount ~ chlor | genus , data=fullsetgroup3 , pch=20 , cex=3 , col=rgb(0.2,0.4,0.8,0.5) )




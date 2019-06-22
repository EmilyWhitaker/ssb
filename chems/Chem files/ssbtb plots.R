library(lubridate)
library(ggplot2)
library(ggridges)
library(tidyverse)

dataframe1 = read_csv('../ssbtbchem.csv') %>% mutate(as.Date(Date,'%m/%d/%Y'))

dataframe1
## month, date, lake, TIC?, depth

try1 <-ggplot(dataframe1, aes(x=Cl, y=Depth, color=month)) +
  geom_point()
try1

try2 <-ggplot(dataframe1, aes(x=DIC, y=Depth, color=month))+
  geom_line()+
  scale_y_reverse()
try2

## need to group depths starting with 0. also group months.

group_by(dataframe1$month, "")



plotLayer <- function(dataframe1,layername,minDepth,maxDepth){
  plot(dataframe1$Date,dataframe1$DIC,pch= 16 ,cex=1, col = adjustcolor('grey50',0.5),
       main = paste0(lakename,', ',layername,': ',minDepth,'-',maxDepth,' m'),
       xlab = '',ylab = 'ChlA')
  points(dataframe1$Date[dataframe1$group == TRUE],dataframe1f$DIC[dataframe1$group == TRUE],pch= 16 ,cex=1, col = adjustcolor('red3',0.5))
}

plotLayer()

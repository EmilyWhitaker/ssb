library(tidyverse)
library(lubridate)
library(ggplot2)

abc <- ggplot(data = monthdateslides, mapping = aes(x = monthdateslides$year,"%y-%m", y = cellcount)) + 
  geom_point(aes(color= genus)) +
  ggtitle("Sparkling Algal Matter")+
  ylim(0, 2000)+
  xaxt = "n"

# Define the position of tick marks
v1 <- c(1981, 1997, 1998, 1999, 2000, 2001, 2002, 2003)

# Define the labels of tick marks
v2 <- c("1981","1997", "1998", "1999", "2000", "2001", "2002", "2003")  

abc <- axis(side = 1, 
            at = v1, 
            labels = v2,
            tck=-.05)

# Define the position of tick marks
v1 <- c(1981, 1997, 1998, 1999, 2000, 2001, 2002, 2003)

# Define the labels of tick marks
v2 <- c("1981","1997", "1998", "1999", "2000", "2001", "2002", "2003")

ggplot(dataset_yrsdate, aes(year, cellcount)) +
  geom_point(aes(color= genus)) 


ggplot(data=dataset_yrsdate, aes(x=month, y=cellcount, group= year)) +
  geom_line()+
  geom_point(aes(color= genus))+
  xlim(1995, 2003)

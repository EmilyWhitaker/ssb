
library(tidyverse)
library(lubridate)
library(ggplot2)

p = ggplot() + 
  geom_point(data = NDlte_lite3_chloro, aes(x = year4.y, y = CellBioVol), color = "blue") +
  geom_point(data = Adino_lite3_chloro, aes(x = year4.y, y = CellBioVol), color = "red") +
  geom_point(data = Dinobryon_lte_lite3_chloro, aes(x = year4.y, y = CellBioVol), color = "green") +
  geom_point(data = CfKT_lte_lite3_chloro, aes(x = year4.y, y = CellBioVol), color = "purple") +
  geom_point(data = SMCO_lte_lite3_chloro, aes(x = year4.y, y = CellBioVol), color = "orange") +
  xlab('Year') +
  ylab('Biovolume')+
  xlim=c(1995,2010)
print(p)
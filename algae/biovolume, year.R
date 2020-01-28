
library(tidyverse)
library(lubridate)
library(ggplot2)

p = ggplot() + 
  geom_point(data = Adino_lite3_chloro, aes(x = year4frac, y = CellBioVol), color = "red") +
  geom_point(data = Dinobryon_lte_lite3_chloro, aes(x = year4frac, y = CellBioVol), color = "green") +
  geom_point(data = CfKT_lte_lite3_chloro, aes(x = year4frac, y = CellBioVol), color = "purple") +
  geom_point(data = SMCO_lte_lite3_chloro, aes(x = year4frac, y = CellBioVol), color = "orange") +
  geom_point(data = NDlte_lite3_chloro, aes(x = year4frac, y = CellBioVol), color = "blue") + 
  xlab('Year') +
  ylab('Biovolume')+
  xlim(1997,2010)
## need to legand, and conect those dots!!
print(p)
ggsave(plot=p,filename='intimeBiovolumes.png',height = 18, width =16, units = 'in')


geom_point(data = NDlte_lite3_chloro, aes(x = year4frac, y = CellBioVol), color = "blue") +
  
geom_line(data = NDlte_lite3_chloro, aes(x = year4frac, y = CellBioVol), color = "blue")


legend(1, 95, legend=c("ND", "AD", "Dino", "CfKt", "SMCO"),
       col=c("blue", "red", "green", "purple", "orange"))
legend(1, 95, legend=c("ND", "AD", "Dino", "CfKt", "SMCO"),
       col=c("blue", "red", "green", "purple", "orange"))
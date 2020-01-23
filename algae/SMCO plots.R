library(tidyverse)
library(lubridate)
library(ggplot2)

#method to color max point through out? or date?

SMCOatzero2 <- read_csv('smcoatzero.csv')
lte_lite <- read_csv('lte_lite.csv',
                     col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))

bothchloro<- read_csv('fullchloro.csv')


SMCO_lte_lite3 <- lte_lite %>%
  right_join(SMCOatzero2, by= c('sampledate'))
write.csv(SMCO_lte_lite3, 'SMCO_lte_lite3.csv')

SMCO_lte_lite3_chloro <-SMCO_lte_lite3 %>%
  right_join(bothchloro, by= c('sampledate'))
write.csv(SMCO_lte_lite3_chloro, 'SMCO_lte_lite3_chloro.csv')
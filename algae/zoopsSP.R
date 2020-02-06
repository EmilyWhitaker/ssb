library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(readr)
dataset <- read_csv("lter_zoopdata_all.csv", 
                    col_types = cols(station = col_skip()))
View(dataset)
Zoopdata <-read_csv("lter_zoopdata_all.csv", 
                    col_types = cols(station = col_skip())), filter(lakeid == 'SP')
Zoopdata

Zoopdata_SP <- read_csv("zoop_allnl_summary_snap.csv")
Zoopdata_SP

ZooTotals <-  read_csv('zoop_allnl_summary_snap.csv',
                                  col_types = cols(sample_date = col_date(format = "%m/%d/%Y")))
ZooTotals


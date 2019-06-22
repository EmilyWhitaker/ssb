library(tidyverse)
library(lubridate)
library(ggplot2)

slides_date <- 
  dataset %>%
  separate(sampledate, c("year", "month", "day"), sep= "-", extra = "merge")

slides_combo <- as.yearmon(paste(slides_date$year, slides_date$month), "%Y %m")

slides_date2 <- as.yearmon(dataset(sampledate))

slides_date <-
  slides_date %>%
  mutate(
    month = as.numeric(month) 
  )

slides_date <-
  slides_date %>%
  mutate(
    year = as.numeric(year) 
  )


names(slides_date)

ggplot(data = slides_date, mapping = aes(x = year(), y = cellcount)) + 
  geom_point(aes(color= genus)) +
  ggtitle("Sparkling Algal Matter")
  

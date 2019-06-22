## looking for trends

groupped = read_csv('~/Documents/GitHub/Sparkling/algae/SFS/groupped.csv') %>% 
  mutate(sampledate = as.Date(sampledate,'%m/%d/%Y'))

groupped$group = factor(groupped$group,levels = c('iceon surface','iceon depth','iceoff surface','iceoff depth'),ordered=T)
chlorobox <- ggplot(data=groupped, aes(x = as.character(year), y = `from group`))+
  geom_boxplot(aes(fill=group)) +
  scale_fill_manual(values = c('#3300CC', '#99CCFF','#CC0000','#FF6666'),
                    name = "", labels = c("Surface ice on", "Integrated ice on","Surface no-ice","Integrated no-ice" )) +
  labs(y="Chlorophyll (μg/l)", x = "Year") +
  theme_gray(base_size = 18) +
  theme(legend.position = c(0.93,0.94)) 
chlorobox

weather = read_csv('~/Documents/GitHub/Sparkling/algae/SFS/winterSPdata.csv') %>% 
  mutate(sampledate = as.Date(sampledate,'%m/%d/%Y'))
weather

chloroweather <- groupped %>%
  full_join(weather,by = c('lakeid','sampledate','year'))
chloroweather

chloroweather$group = factor(chloroweather$group,levels = c('iceon surface','iceon depth','iceoff surface','iceoff depth'),ordered=T)
chloroweatherbox <- ggplot(data=chloroweather, aes(x = as.character(year), y = `from group`))+
  geom_boxplot(aes(fill=group)) +
  scale_fill_manual(values = c('#3300CC', '#99CCFF','#CC0000','#FF6666'),
                    name = "", labels = c("Surface ice on", "Integrated ice on","Surface no-ice","Integrated no-ice" )) +
  labs(y="Chlorophyll (μg/l)", x = "Year") +
  xlim=c(1997,2003) +
  theme_gray(base_size = 18) +
  theme(legend.position = c(0.93,0.94)) 
chloroweatherbox


chloroweather$group = factor(chloroweather$group,levels = c('iceon surface','iceon depth','iceoff surface','iceoff depth'),ordered=T)
chloroweather2 <- ggplot(data=groupped, aes(x = as.character(year), y = `from group`))+
  geom_boxplot(aes(fill=group)) +
  scale_fill_manual(values = c('#3300CC', '#99CCFF','#CC0000','#FF6666'),
                    name = "", labels = c("Surface ice on", "Integrated ice on","Surface no-ice","Integrated no-ice" )) +
  labs(y="Chlorophyll (μg/l)", x = "Year") +
  theme_gray(base_size = 18) +
  theme(legend.position = c(0.93,0.94)) 
chloroweatherbox

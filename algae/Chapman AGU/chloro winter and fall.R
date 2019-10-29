#merge chloro with snowince data plot 
chlor = read_csv('ntl35_v3.csv')
winter = read_csv('ntl34_v5.csv') 
  # filter(lakeid %in% c('AL','BM','CB','CR','SP','TB','TR'))
  

chlorwinter = chlor %>% left_join(winter) %>% 
  filter(lakeid == 'SP') %>% 
  filter(depth <= 1) %>% 
  mutate(group = ifelse(!is.na(totice), 'ice','no ice'))

chloroplot <- ggplot(chlorwinter, aes(x=sampledate, y= chlor)) +
  geom_point() +
  geom_point(aes(y = avsnow), color = 'red3') +
  geom_point(aes(y = totice), color = 'navy') +
  # geom_line() +
  theme_bw()
chloroplot

ggplot(chlorwinter) +
  geom_point(aes(x=sampledate, y= chlor, color = avsnow),
             pch = 16) +
  scale_color_viridis_c() +
  xlab('') + ylab('Chlorophyll (ug/L)') +
  theme_bw()

meow<- ggplot(chlorwinter) +
  geom_boxplot(aes(x = group, y = chlor), fill = 'slategrey') +
  xlab('Ice on (n = 121) vs. Ice off (n = 576)') + ylab('Chlorophyll (ug/L)') +
  theme_bw()
meow

ggsave(plot=meow,filename='fig2.png',height = 6, width =10, units = 'in')

#SAVE THIS EMILY



table(chlorwinter$group)
#need to change colors and add second axis

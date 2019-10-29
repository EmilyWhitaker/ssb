
###yay!!-- genra 

sheet11 = read_csv('youchangedthis.csv') 

sheet 

sheet11$group = factor(sheet11$group,levels = c('iceon','iceoff'),ordered=T)


genusyearplot2 <- ggplot(sheet11, aes(Genus, sheet11$CellBioVol))+
  geom_boxplot(aes(fill= group)) +
  scale_y_continuous(trans='log2')+
  labs(y="Biovolume", x = "Genus") +
  theme_bw(base_size = 18) +
  scale_fill_manual(values = c('#99CCFF','#FF6666'),
                    name = "", labels = c("ice on",  "ice off")) +
  theme(axis.text.x = element_text(angle=90, vjust=0, hjust=0)) +
  facet_wrap(sheet11$month, ncol=1)
  
genusyearplot2
ggsave(plot=genusyearplot2,filename='generabiov4.png',height = 18, width =16, units = 'in')

##yea!



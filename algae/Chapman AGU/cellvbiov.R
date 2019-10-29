#cell bio info


cbg2 = read_csv('cellbiogroup.csv') %>% as.character(year(), month())

cbg$group = factor(cbg$group,levels = c('iceon TotalCellCount','iceon TotalBiovolume','iceoff TotalCellCount','iceoff TotalBiovolume'),ordered=T)

#need to changeformating and colors
cbgroup<- ggplot(data = cbg, aes(x = year, y = cbg$FromGroup))+
  geom_boxplot(aes(fill=cbg$Genus)) + scale_y_continuous(trans='log2')+
  scale_fill_manual(values = c('#3300CC', '#99CCFF','#CC0000','#FF6666'))+
  name = "", labels = c("iceon TotalCellCount","iceon TotalBiovolume","iceoff TotalCellCount","iceoff TotalBiovolume")) +
  theme_bw(base_size = 18) 
cbgroup



par(mfrow=c(1,2))
boxplot($year, $BioVolume, fil=group, main="Scatterplot of wt vs. mpg")
boxplot($year, $CellCoount, fil=group, main="Scatterplot of wt vs disp")


cbg = read_csv('cellbiogroup.csv') %>% select(group:`End XY`) #get rid of extra columns 

bvonoff$group = factor(bvonoff$group,levels = c('iceon','iceoff'),ordered=T)

#need to changeformating and colors

ggplot(data = cbg) +
  geom_boxplot(aes(x = as.character(year), y = FromGroup, fill = Genus)) +
  scale_y_continuous(trans='log2') +
  scale_fill_manual(values = c('#3300CC', '#99CCFF','#CC0000','#FF6666'), name = "") +
  ylab('Biovolume/Cell Count') + xlab('year') +
  theme_bw(base_size = 18) +
  facet_wrap(~plot, ncol=2)

bvon <- ggplot(data = bvonoff) +
  geom_boxplot(aes(x = as.character(year), y = FromGroup, fill = group)) +
  scale_y_continuous(trans='log2') +
  scale_fill_manual(values = c('#99CCFF','#FF6666'), name = "") +
  ylab('Biovolume/Cell Count') + xlab('year') +
  theme_bw(base_size = 18) 
bvon

cellon <- ggplot(data = cconoff) +
  geom_boxplot(aes(x = as.character(year), y = FromGroup, fill = group)) +
  scale_y_continuous(trans='log2') +
  scale_fill_manual(values = c('#99CCFF','#FF6666'), name = "") +
  ylab('Biovolume/Cell Count') + xlab('year') +
  theme_bw(base_size = 18) 
cellon


par(mfrow = c(2,1))
bvon <- ggplot(data = bvonoff) +
  geom_boxplot(aes(x = as.character(year), y = FromGroup, fill = group)) +
  scale_y_continuous(trans='log2') +
  scale_fill_manual(values = c('#99CCFF','#FF6666'), name = "") +
  ylab('Biovolume/Cell Count') + xlab('year') +
  theme_bw(base_size = 18) 
bvon

cellon <- ggplot(data = cconoff) +
  geom_boxplot(aes(x = as.character(year), y = FromGroup, fill = group)) +
  scale_y_continuous(trans='log2') +
  scale_fill_manual(values = c('#99CCFF','#FF6666'), name = "") +
  ylab('Biovolume/Cell Count') + xlab('year') +
  theme_bw(base_size = 18) 
cellon


















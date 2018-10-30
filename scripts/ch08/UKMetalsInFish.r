metals.in.fish<-read.csv('uk.metals.in.fish.txt',sep='\t')
descr<-c('Metal contaminants (mg/kg wet weight)',
'are analysed in fish muscle.',
'Pesticides and PCBs are analysed in fish liver.',
'Total DDT = ppDDE + ppTDE + ppDDT.',
'Total HCH = a HCH + g HCH.',
'PCBs are measured on a formulation basis (as Arcolor 1254).',
'For 1993 data, only larger fish were available.',
'Source publication:',
'e-Digest of Environmental Statistics, ',
'Published September 2003 ',
'Department for Environment, ',
'Food and Rural Affairs ',
'http://www.defra.gov.uk/environment/statistics/index.htm ')
uk.metals.in.fish<-list()
uk.metals.in.fish[[1]]<-descr
metals.in.fish<-read.csv('uk.metals.in.fish.txt',sep='\t')
uk.metals.in.fish[[2]]<-metals.in.fish
uk.metals.in.fish[[2]][71,4]<-NA
uk.metals.in.fish[[2]][,4]<-
   as.numeric(uk.metals.in.fish[[2]][,4])
save(uk.metals.in.fish,file='uk.metals.in.fish')
uk<-uk.metals.in.fish[[2]]
windows(width = 3, height = 3, pointsize = 8)
boxplot(uk[3:6])

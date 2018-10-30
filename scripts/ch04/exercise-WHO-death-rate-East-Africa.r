who<-
   read.csv('who.by.continents.and.regions.txt',
   sep='\t')
tst<-who$Region=='Eastern Africa'
temp<-data.frame(country=who$country[tst],
   dr=who$dr[tst])
temp<-temp[!is.na(temp$dr),]
idx<-sort(temp1$dr,index.return=TRUE)
data.frame(country=temp$country[idx$ix],dr=temp$dr[idx$ix],
   pr=temp$dr[idx$ix]/1000)
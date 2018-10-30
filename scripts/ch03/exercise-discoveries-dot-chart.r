data(discoveries)
discover<-as.integer(discoveries)
period<-factor(c(rep('1860-1879',20),
   rep('1880-1899',20),
   rep('1900-1919',20),
   rep('1920-1939',20),
   rep('1940-1959',20)))
discover<-data.frame(period,discover)
means<-tapply(discover$discover,discover$period,mean)
windows(width = 3, height = 3, pointsize = 8)
dotchart(means,xlab='discoveries')
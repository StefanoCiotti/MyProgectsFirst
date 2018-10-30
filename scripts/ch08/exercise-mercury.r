mercury<-uk[uk$Metal=='Mercury',c(1,3:5)]
print(c(cod=mean(mercury[,2],na.rm=TRUE),
   plaice=mean(mercury[,3],na.rm=TRUE),
   whiting=mean(mercury[,4],na.rm=TRUE)))
print(c(cod=median(mercury[,2],na.rm=TRUE),
   plaice=median(mercury[,3],na.rm=TRUE),
   whiting=median(mercury[,4],na.rm=TRUE)))

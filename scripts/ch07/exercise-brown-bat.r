sex<- c('male','female','male','female')
state<-c(rep('New Mexico',2),rep('Indiana',2))
n<-c(5,3,6,40)
average<-c(8.47,6.96,6.03,6.99)
sd<-c(0.81,0.27,NA,NA)

brown.bat<-data.frame(state,sex,n,average,sd)

brown.bat[3,5]<-0.81
brown.bat[4,5]<-0.27

p1<-pnorm(8,brown.bat[,4],brown.bat[,5])
p2<-pnorm(7.5,brown.bat[,4],brown.bat[,5]) - 
   pnorm(6.5,brown.bat[,4],brown.bat[,5])

brown.bat<-data.frame(brown.bat,p1,p2)
names(brown.bat)[c(6,7)]<-c('P(X<8)','P(6.5<X<7.5)')

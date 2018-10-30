library(gtools)
x<-c(5,3,3,4,4)
comb<-data.frame(combinations(5,3,x,set=FALSE))
average<-median<-statistic<-vector()
for(i in 1:length(comb[,1])){
   average[i]<-mean(t(comb[i,]))
   median[i]<-median(t(comb[i,]))
   statistic[i]<-(max(t(comb[i,]))+min(t(comb[i,])))/2
}
stats<-data.frame(average,median,statistic)
print(stats)
print(table(average)/10)
print(table(median)/10)
print(table(statistic)/10)
print(c(mu=mean(x),mean=mean(average),median=mean(median),
   statistic=mean(statistic)))


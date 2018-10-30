rm(list=ls())
dbh <- read.table('DBH.txt', header = FALSE, sep = '\t')
names(dbh) <- c('DBH', 'height', 'age')
attach(dbh)

openg(4.5,2.75)
par(mfrow=c(1,2))
plot(age,height)
x<-log(age);y<-log(height)
plot(log(age),log(height))
model<-lm(y~x)
abline(reg=model)
saveg('trees-height-vs-age-full-data',4.5,2.75)
print(summary(model))


ey<-predict(model)

openg(5,5)
plot(x,y,xlim=c(1,5),ylim=c(0,3.5),xlab='log(age)',
   ylab='log(height)')
abline(reg=model)
for(i in 1:length(x))
   lines(c(x[i],x[i]),c(y[i],ey[i]))
points(x,ey,pch=19)
text(x,ey,labels=round(ey,3),pos=4)
text(x,y,labels=round(y,3),pos=4)
for(i in 1:length(x)){
   if(i==2 | i==4) pos=2 else pos=4
   text(x[i],ey[i]-(ey[i]-y[i])/2,
      labels=bquote(e[.(i)]==.(round(y[i]-ey[i],3))),pos=pos)
}
saveg('trees-height-vs-age',5,5)

d<-data.frame(x,y,ey,y-ey,(y-ey)^2)
names(d)<-c('~~~~$x$~~~~','~~~~$y$~~~~','~~~~$E[y|x]$~~~~','~~~~$e$~~~~','~~~~$e^2$~~~~')
print(sum((y-ey)^2))

library(Hmisc)

latex(d,
   file='trees-height-vs-age.tex',
   label='table: DBH error SSQ',
   cdec=rep(3,5), 
   col.just=rep('r',5),
#   rowname=c('Death (all cause)','Cancer Death',
#      'Relapse','Hospitalization'),
#   rgroup=c('Fatal Events','Non--fatal Events'),
   rowlabel='$i$', 
   caption='log tree height ($y$) vs log age ($x$) and the expected values of y according to (\\protect\\ref{eq: E[y] for height vs age}).',
   na.blank=TRUE,
   landscape=FALSE,
   where="!htbp",
   ctable=TRUE)

v<-var(d[,1:2])
b1<-v[1,2]/v[1,1]
b0<-mean(d[,2])-b1*mean(d[,1])

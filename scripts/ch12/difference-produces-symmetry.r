
windows()
par(cex=2,font=6,font.axis=6,font.lab=8)
x<-seq(-1,10,length=2000)
plot(x,dgamma(x,2)-dgamma(x+3,2),axes=T,type='l',xlab='',
   ylab='',ylim=c(-1,1))
abline(h=0)
lines(x,dgamma(x,2))
lines(x,dgamma(x+3,2))
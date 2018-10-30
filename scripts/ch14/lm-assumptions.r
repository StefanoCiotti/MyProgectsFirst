rm(list=ls())

xx<-seq(-2,2,length=101)
yy<-dnorm(xx,0,.5)
s<-1:100

a<-1; b<-1; r<-21;c<-5;
x<-y<-z<-matrix(ncol=c,nrow=r)
for(j in 1:c) x[,j]<-seq(j,j,length=r) 
for(j in 1:c) z[,j]<-seq(-2.5,2.5,length=r)
for(j in 1:c) {
   set.seed(j)
   y[,j]=a+rnorm(z[,j],b*x[1,j],0.75)
}

openg()
plot(x[,1],y[,1],xlim=c(0,6),ylim=c(0,10),
   xlab='x',ylab='Y',axes=FALSE)
for(j in 1:c) {
   if(j>1) points(x[,j],y[,j])
   polygon(x[1,j]+yy[s],a+b*x[1,j]+xx[s])
   lines(c(x[1,j],x[1,j]+yy[50]),rep(a+b*x[1,j]+xx[50],2))

}
box(); abline(a,b)
saveg('lm-assumptions')
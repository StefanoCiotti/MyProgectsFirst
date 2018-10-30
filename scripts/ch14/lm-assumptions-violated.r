rm(list=ls())

openg(5,2.75)
par(mfrow=c(1,2))

#increasing variance
a=1; b=1; r=20;c=5;
x=matrix(ncol=c,nrow=r)
y=matrix(ncol=c,nrow=r)
z=matrix(ncol=c,nrow=r)
for(j in 1:c) x[,j]=seq(j,j,length=r)
for(j in 1:c) z[,j]=seq(-2.5,2.5,length=r)
for(j in 1:c) y[,j]=a+rnorm(z[,j],b*x[1,j],x[1,j]*.75)
plot(x[,1],y[,1],xlim=c(0,5),ylim=c(0,10),
   xlab='x',ylab='Y')
for(j in 2:c) points(x[,j],y[,j])
abline(a,b)

#not independent
a=1; b=1
r=20;c=5;
x=matrix(ncol=c,nrow=r)
y=matrix(ncol=c,nrow=r)
z=matrix(ncol=c,nrow=r)
t=sin(seq(1,5,length=5)*2*pi/5)
for(j in 1:c) x[,j]=seq(j,j,length=r)
for(j in 1:c) z[,j]=seq(-2.5,2.5,length=r)
for(j in 1:c) y[,j]=a+t[j]+rnorm(z[,j],b*x[1,j],.75)
plot(x[,1],y[,1],xlim=c(0,5),ylim=c(0,10),
   xlab='x',ylab='')
for(j in 2:c) points(x[,j],y[,j])
abline(a,b)
saveg('lm-assumptions-violated',5,2.75)

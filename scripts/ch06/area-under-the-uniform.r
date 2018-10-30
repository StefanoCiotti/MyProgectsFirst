x <- c(seq(-3, 3, length = 1000)) ; y <- dnorm(x)
par(mfrow = c(1, 3))
x<-c(seq(2,8,length=1000))
plot(x,dunif(x,4,6),xlim=c(2,8),type='s',
   xlab=expression(italic(x)),
   ylab=expression(italic(P(X==x))),
   main=expression(italic(P(X<=6)-P(X<=4))))
x<-c(4,4,6,6,4);y<-c(0,.5,.5,0,0)
polygon(x,y,col='grey90')

x<-c(seq(2,8,length=1000))
plot(x,dunif(x,4,6),xlim=c(2,8),type='s',
   xlab=expression(italic(x)),ylab='',
   main=expression(italic(P(X<=5.5)-P(X<=4.5))))
x<-c(4.5,4.5,5.5,5.5,4.5);y<-c(0,.5,.5,0,0)
polygon(x,y,col='grey90')

x<-c(seq(2,8,length=1000))
plot(x,dunif(x,4,6),xlim=c(2,8),type='s',
   xlab=expression(italic(x)),ylab='',
   main=expression(italic(1-P(X<=5.5))))
x<-c(5.5,5.5,6,6,5.5);y<-c(0,.5,.5,0,0)
polygon(x,y,col='grey90')

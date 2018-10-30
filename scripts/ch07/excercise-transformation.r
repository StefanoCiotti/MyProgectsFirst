set.seed(1)
x<-rexp(40)
h<-w<-4.5
openg(height=h,width=w)
par(mfrow=c(2,2))
hist(x,freq=FALSE,ylab='density',main='')
qqnorm(x,main='data')
qqline(x)
qqnorm(log(x),main='log(data)')
qqline(log(x))
qqnorm(sqrt(x),main='sqrt(data)')
qqline(sqrt(x))
saveg('exercise-data-transformation',height=h,width=w)

options(digits=3)
print(x)

0.7552 1.1816 0.1457 0.1398 0.4361 2.8950 1.2296 0.5397
0.9566 0.1470 1.3907 0.7620 1.2376 4.4239 1.0545 1.0352
1.8760 0.6547 0.3369 0.5885 2.3645 0.6419 0.2941 0.5659
0.1061 0.0594 0.5787 3.9589 1.1733 0.9968 1.4353 0.0373
0.3240 1.3205 0.2035 1.0227 0.3017 0.7252 0.7515 0.2350
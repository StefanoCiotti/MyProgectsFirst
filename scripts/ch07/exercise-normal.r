x<-seq(-3,3,length=101)
y<-pnorm(x)
openg()
plot(x,y)
saveg('exercise-pnorm')

y<-dnorm(x)
openg()
plot(x,y)
saveg('exercise-dnorm')

seed<-2
set.seed(seed)
y<-rnorm(100)
print(mean(y)) ; print(sd(y))
set.seed(seed)
y<-rnorm(1000)
print(mean(y)) ; print(sd(y))


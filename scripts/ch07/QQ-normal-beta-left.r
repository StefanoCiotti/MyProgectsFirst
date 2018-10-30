rm(list = ls())
par(mfrow = c(1, 2))
set.seed(1) ; x <- rbeta(101, 1.5, 3)
h(x, xlab = 'x', ylim = c(0, 3)) ;
y <- seq(0, 1, length = 101)
lines(y, dnorm(y, .5, .15), type = 'l')
qqnorm(x, main='') ; qqline(x)

load('terror.by.Hamas.rda')
terror <- terror.by.Hamas
lambda <- 1 / mean(terror$Killed) ;
x <- 0 : 25
h(terror$Killed, xlab = 'killed by Hamas')
lines(x, dexp(x, lambda))
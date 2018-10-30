load('terror.by.Hamas.rda')
terror <- terror.by.Hamas
lambda <- 1 / mean(terror$Killed)
j1 <- terror$Julian ; j2 <- j1
j1 <- j1[-length(j1)] ; j2 <- j2[-1]
h(j2 - j1, xlab = 'days between attacks')
x <- 0 : 120
lines(x, dexp(x, 1 / mean(j2 - j1)))

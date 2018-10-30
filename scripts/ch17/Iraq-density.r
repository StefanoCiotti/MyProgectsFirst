rm(list = ls())
library(CTFS)
load('iraq.dead.hostile.rda')
load('iraq.injured.rda')
load('iraq.daily.deaths.rda')

openg(4.5)
# injured
par(mfrow = c(1, 2), mar = c(4,4,1,0) + 0.1)
d <- iraq.injured$Injured
injured.h <- h(d, xlab = 'Injured')
x <- 0:1600
mu <- mean(d); 
size <- mu^2/(var(d) - mu)
lines(x, dnbinom(x, size, , mu))

x <- injured.h$counts
y <- dnbinom(injured.h$mids, size, , mu)
xy <- cbind(x, y)
cst <- chisq.test(xy, correct = FALSE,
  simulate.p.value = TRUE, B = 2000)
print(cst)

# dead
dead <- iraq.dead.hostile
period <- 7
(b <- ceiling((dead$Julian[length(dead$Julian)] - 
  dead$Julian[1]) / 7))
cj <- cut(dead$Julian[dead$Where == 'Baghdad'], b)
counts <- table(cj)

par(mar = c(4,2,1,0) + 0.1)

# dead, negative binomial
dead.h <- h(counts, xlab = 'Dead')
x <- 0:70
mu <- mean(counts); 
size <- mu^2/(var(counts) - mu)
lines(x, dnbinom(x, size, , mu))

x <- dead.h$counts
y <- dnbinom(dead.h$mids, size, , mu)
xy <- cbind(x, y)
cst <- chisq.test(xy, correct = FALSE,
  simulate.p.value = TRUE, B = 2000)
print(cst)

#dead, Poisson
dead.h <- h(counts, xlab = 'Dead')
x <- 0:70
mu <- mean(counts); 
lines(x, dpois(x, mu))

x <- dead.h$counts
y <- dpois(dead.h$mids, mu)
xy <- cbind(x, y)
cst <- chisq.test(xy, correct = FALSE,
  simulate.p.value = TRUE, B = 2000)
print(cst)

#saveg('../docs/Iraq-densities', 4.5)



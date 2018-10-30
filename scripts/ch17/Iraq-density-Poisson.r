rm(list = ls())
library(CTFS)
load('iraq.dead.hostile.rda')
load('iraq.injured.rda')
load('iraq.daily.deaths.rda')

openg(4.5)
# injured
par(mfrow = c(1, 2), mar = c(4,4,1,0) + 0.1)
d <- iraq.injured$Injured
injured.h <- hist(d, plot = FALSE)
plot(injured.h$mids, injured.h$intensities * sum(d),
  pch = 19, cex = 1.25, xlab = 'Injured', ylab = 'Expected')
abline(h = 0)
x <- injured.h$mids
mu <- mean(d); 
size <- mu^2/(var(d) - mu)
compare <- data.frame(empirical = injured.h$density,
  theoretical = dpois(x, mu))
lines(injured.h$mids, compare$theoretical * sum(d), 
  type = 'h', lwd = 2)

xy<- cbind(injured.h$intensities * sum(d),compare$theoretical * sum(d))
cst <- chisq.test(xy, correct = FALSE,
  simulate.p.value = TRUE, B = 2000)
print(cst)


# dead
d <- iraq.daily.deaths
j <- i <- 1 ; v <- vector()
while (j <= length(d[,1])){
  v[i] <- sum(d$Count[j : (j + 6)])
  i <- i + 1 ; j <- j + 7
}
if( is.na( v[length(v)] ) ) v <- v[-length(v)]
dead.h <- hist(v, plot = FALSE)
par(mar = c(4,2,1,1) + 0.1)
plot(dead.h$mids, dead.h$intensities * sum(v),
  pch = 19, cex = 1.24, ylab = '', xlab = 'Dead')
abline(h = 0)
x <- dead.h$mids
mu <- mean(v); 
size <- mu^2/(var(v) - mu)
compare <- data.frame(empirical = dead.h$density,
  theoretical = dpois(x, mu))
lines(dead.h$mids, compare$theoretical * sum(v), type = 'h',
  lwd = 2)

xy<- cbind(dead.h$intensities * sum(v),compare$theoretical * sum(v))
cst <- chisq.test(xy, correct = FALSE,
  simulate.p.value = TRUE, B = 2000)
print(cst)

saveg('../docs/Iraq-densities-Poisson', 4.5)



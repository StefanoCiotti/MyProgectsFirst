rm(list = ls())
library(UsingR) ; data(south) ; n <- c(5, 10, 100)
lambda <- mean(south) ; par(mfrow = c(2, 2))

openg(4.5, 4.5)
par(mfrow = c(2, 2))
h(south, xlab = 'murder rate', ylim = c(0, 0.1))
x <- 0 : 30 ; lines(x, dpois(x, lambda), type = 'h', lwd = 2)
abline(h = 0, lwd = 2)

set.seed(100)
ylab = c('', 'density', '')
for(i in 1 : 3){
  m <- matrix(rpois(n[i] * 500, lambda), nrow = n[i],
    ncol = 500)
  l <- apply(m, 2, mean)
  h(l, ylim = c(0, 1.1), xlim = c(10, 18),
    xlab = bquote(italic(list(l,~~ n==.(n[i])))),
    ylab = ylab[i])
  x <- seq(0, 20, length = 201)
  lines(x, dnorm(x, lambda, sqrt(lambda/n[i])))
}
saveg('sampling-density-intensity-murders-south', 4.5, 4.5)


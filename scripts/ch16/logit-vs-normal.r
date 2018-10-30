rm(list = ls())

logit <- function(p){log(p / (1 - p))}
p <- seq(.01, .99, length = 101)
plot(logit(p), p, type = 'l', xlim = c(-4, 4))
x <- seq(-6, 6, length = 101)
lines(x, pnorm(x), lwd = 3)

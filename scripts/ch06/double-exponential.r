rdouble.exp <- function(n, mu = 0, sigma = 1){
  return(rexp(n, 1 / sigma) *
    ifelse(runif(n) <= 0.5, -1, 1))
}

ddouble.exp <- function(x, mu = 0, sigma = 1){
  return(1 / (2 * sigma) * exp(-abs((x - mu) / sigma)))
}

pdouble.exp <- function(x, mu = 0, sigma = 1){
  return(1/2 * (1 + sign(x - mu) *
    (1 - exp(-abs(x - mu) / sigma))))
}

set.seed(5) ; y <- rdouble.exp(10000)
x <- seq(-5, 5, length = 1001)
par(mfrow = c(1, 2))
h(y, xlab = 'x', ylim = c(0, 0.5))
lines(x, ddouble.exp(x), type = 'l')
mu.hat <- mean(y) ; sigma.hat <- sum(abs(y - mu.hat))/ length(y)
lines(x, ddouble.exp(x, mu.hat, sigma.hat), lwd = 3)

plot(x, pdouble.exp(x), col = 'blue', pch = 21)
lines(ecdf(y))
lines(x,  pdouble.exp(x, mu.hat, sigma.hat), col = 'red')
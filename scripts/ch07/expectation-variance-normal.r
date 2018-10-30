mu <- 10 ; sigma <- 2 ; x <- seq(0, 20, length = 101)
set.seed(4) ; X <- rnorm(100, mu, sigma)
h(X, xlab = 'x')
lines(x, dnorm(x, mu, sigma), lwd = 3)
lines(x, dnorm(x, mean(X), sd(X)))
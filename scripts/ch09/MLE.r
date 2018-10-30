rm(list = ls())
mu <- 20 ; sigma.2 <- 4 ; set.seed(33)
X <- rnorm(100, mu, sqrt(sigma.2))
log.L <- function(mu.hat = 15, sigma.2.hat = 6){
  n <- length(X)
  n / 2 * log(2 * pi * sigma.2.hat) + 
    1/2 * sum((X - mu.hat)^2 / sigma.2.hat)
}
library(stats4)
(fit <- mle(log.L))

confint(profile(fit))
par(mfrow = c(1, 2))
plot(profile(fit))


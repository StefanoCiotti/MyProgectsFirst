rm(list = ls())
par(mfrow = c(1, 2))
load('casualties.rda')
h(d <- diff(casualties$Julian), xlab = 't')

set.seed(10)
m <- matrix(sample(d, 30 * 10000, replace = TRUE),
  nrow = 30, ncol = 10000)
h(apply(m, 2, mean), xlab = 'mean')
x <- seq(0, 1.2, length = 201)
lines(x, dnorm(x, mean(d), sd(d) / sqrt(30)))


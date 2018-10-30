rm(list = ls())
par(mfrow = c(2, 2))
set.seed(200)

# 1. no relation
X <- rnorm(20, 0, .25) ; Y <- rnorm(20, 0, .25)
plot(X, Y, axes = FALSE, xlim = c(-1, 1), ylim = c(-2, 2),
   xlab = '', ylab = '') ; abline(h = 0) ; abline(v = 0)

# 2. positive relation
X <- seq( -1, 1, length = 20) ; Y <- X + rnorm(X, 0, .25)
plot(X, Y, axes = FALSE, xlim = c(-1, 1), ylim = c(- 2, 2),
   xlab = '', ylab = '') ; abline(h = 0) ; abline(v = 0)

# 3. negative relation
Y <-  -X + rnorm(X, 0, .25)
plot(X, Y, axes = FALSE, xlim = c(-1, 1), ylim = c(-2, 2),
   xlab = '', ylab = '') ; abline(h = 0) ; abline(v = 0)

# 4. quadratic relation
Y <-  -.5 + X * X + rnorm(X, 0, .25)
plot(X, Y, axes = FALSE, xlim = c(-1, 1), ylim = c(-2, 2),
   xlab = '', ylab = '') ; abline(h = 0) ; abline(v = 0)
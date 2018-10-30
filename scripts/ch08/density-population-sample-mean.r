rm(list = ls())
DENSITY <- c('E[x]' = 180, 'S[x]' = 10)
POPULATION <- c(mu = 0, sigma = 0)
SAMPLE <- c(mu.hat = 0, sigma.hat = 0)

INFINITE <- 10001 ; N <- 501
x <- seq(120, 240, length = N)
y <- dnorm(x, DENSITY[1], DENSITY[2])
set.seed(79) ; population <- rnorm(N, DENSITY[1], DENSITY[2])
POPULATION[1] <- mean(population)
POPULATION[2] <- sd(population)
n <- 30 ; X <- sample(population, n)
SAMPLE[1] <- mean(X) ; SAMPLE[2] <- sd(X)

plot(x, y, type = 'l',
  xlab = 'x', ylab = 'density')
abline(v = DENSITY[1], lwd = 2, col = 'blue')
abline(v = POPULATION[1], lwd = 4, col = 'red', lty = 2)
abline(v = SAMPLE[1], lty = 3)

compare <- cbind(DENSITY, POPULATION, SAMPLE)
dimnames(compare)[[1]] <- c('mean', 'sd') ; compare


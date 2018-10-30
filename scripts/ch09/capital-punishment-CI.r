rm(list = ls())
load('capital.punishment.rda')
cp <- capital.punishment
age <- (cp[, 26 ] * 12 + cp[, 25] - 
   (cp[, 12] * 12 + cp[, 11])) / 12
mu <-  mean(age, na.rm = TRUE)
sigma <- sd(age, na.rm = TRUE)
round(c(mu = mu, sigma = sigma),2)

head(cp[, c(26, 25, 12, 11)], 3)

set.seed(3) ; n <- 30 ; X <- sample(age, n)
summary(X)

mu.hat <- mean(X) ; S <- sd(X) ; alpha <- 0.05
round(c(low = qnorm(alpha / 2, mu.hat, S / sqrt(n)),
  high = qnorm(1 - alpha / 2, mu.hat, S / sqrt(n))), 2)
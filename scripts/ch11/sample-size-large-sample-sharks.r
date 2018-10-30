rm(list = ls())
sigma <- 0.63 ; alpha.L <- 0.01 ; alpha.H <- 0.025

openg(4.5, 2.5)
par(mfrow=c(1, 2))

# power profile

mu.0 <- 1.06 ; n <- 81 ; SE <- sigma /sqrt(n)
x.L <- qnorm(alpha.L, mu.0, SE)
mu.a <- seq(0.7, mu.0, length = 201)
power.L <- pnorm(x.L, mu.a, SE)
mu.A <- seq(mu.0, 1.4, length = 201)
x.H <- qnorm(1 - alpha.H, mu.0, SE)
power.H <- 1 - pnorm(x.H, mu.A, SE)
plot(mu.a - mu.0, power.L, type = 'l', 
  xlim = c(-0.3, 0.3),
  xlab = expression(italic(Delta*mu)),
  ylab = expression(italic(1-beta)))
lines(mu.A - mu.0, power.H)

# sample size

mu.0 <- 1.06 ; mu.a <- 0.96 ; mu.A <- 1.16
beta.L <- 0.3 ; beta.H <- 0.2
n.L <- ceiling(sigma^2 / (mu.0 - mu.a)^2 *
  (qnorm(alpha.L) + qnorm(1-beta.L))^2)
n.H <- ceiling(sigma^2 / (mu.0 - mu.A)^2 *
  (qnorm(1 - alpha.H) + qnorm(beta.H))^2)
c(n.L = n.L, n.H = n.H, n = n.L + n.H)

# sample size profile

n.l <- n.L ; n.h <- n.H
mu.a <- seq(0.7, mu.0, length = 201)
n.L <- sigma^2 / (mu.0 - mu.a)^2 *
  (qnorm(alpha.L) + qnorm(1-beta.L))^2
mu.A <- seq(mu.0, 1.4, length = 201)
n.H <- sigma^2 / (mu.0 - mu.A)^2 *
  (qnorm(1 - alpha.H) + qnorm(beta.H))^2
plot(mu.a - mu.0, n.L, type = 'l', ylim = c(0, 150), 
  xlim = c(-0.3, 0.3),
  xlab = expression(italic(Delta*mu)),
  ylab = expression(italic(n)))
lines(mu.A - mu.0, n.H)
abline(v = c(0.96 - mu.0, 1.06 - mu.0, 1.16 - mu.0), 
  lty = c(2, 1, 2), lwd = 2)
saveg('sample-size-large-sample-sharks', 4.5, 2.5) 
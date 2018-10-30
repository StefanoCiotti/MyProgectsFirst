rm(list = ls())
delta <- 0.25 ; pi.0 <- 0.5 # max sample size
pi.A <- seq(pi.0 - delta, pi.0 + delta, length = 401)
alpha <- 0.01 ; beta <- 0.1

n.L <- (qnorm(alpha / 2) * sqrt(pi.0 * (1 - pi.0))
  + qnorm(1 - beta) * sqrt(pi.A[pi.A < 0.5] * 
  (1 - pi.A[pi.A < 0.5])) / (pi.0 - pi.A[pi.A < 0.5]))^2
n.H <- (qnorm(1 - alpha / 2) * sqrt(pi.0 * (1 - pi.0))
  + qnorm(beta) * sqrt(pi.A[pi.A > 0.5] * 
  (1 - pi.A[pi.A > 0.5])) / (pi.0 - pi.A[pi.A > 0.5]))^2

d <- cbind(
  delta = c(pi.A[pi.A < 0.5], pi.A[pi.A > 0.5]) - 0.5,
  n = c(n.L + n.H, n.L + n.H))

openg()
plot(d, type = 'l', ylog = TRUE,
  ylim = c(0, 50000), xlim = c(-0.02, 0.02),
  xlab = expression(italic(pi[0]-pi[A])),
  ylab = expression(italic(n)))
abline(v = c(-0.005, 0.005), lty = 2) ; abline(h = 16500)
saveg('binomial-n-large-sample-cancer')
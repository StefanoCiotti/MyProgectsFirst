rm(list = ls())
openg(4.5, 2.5)
mu.0 <- 18 ; S <- 1.23 ; n <- 48 ; SE <- S / sqrt(n)
mu.A <- seq(18, 19, length = 201) ; alpha = 0.05
x.H <- qnorm(1-alpha, mu.0, SE)
beta <- pnorm(x.H, mu.A, SE)
par(mfrow=c(1,2))
plot(mu.A, beta, type = 'l', 
   xlab = expression(italic(mu[A])),
   ylab = expression(beta))
plot(mu.A, 1 - beta, type = 'l', 
   xlab = expression(italic(mu[A])),
   ylab = 'power')
saveg('power-viagra-3', 4.5, 2.5)
alpha <- 2 ; sigma <- 2
set.seed(10) ; r.gamma <- rgamma(1000, alpha, scale = sigma)
x <- seq(0, 20, length = 201)
par(mfrow = c(1, 2))
h(r.gamma, xlab = 'x', ylim = c(0, 0.2))
lines(x, dgamma(x, alpha, scale = sigma), type = 'l')
m <- mean(r.gamma) ; v <- var(r.gamma)
sigma.hat <- v / m ; alpha.hat <- m^2 / v
lines(x, dgamma(x, alpha.hat, scale = sigma) , lwd = 3)

plot(x, pgamma(x, alpha, scale = sigma),
  col = 'blue', pch = 21)
lines(ecdf(r.gamma))
lines(x,  pgamma(x, alpha.hat, scale = sigma.hat),
  col = 'red')
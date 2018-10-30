rm(list = ls())
lambda.0 <- 0.35 ; lambda.A <- seq(0.35, 0.85, length = 501)
alpha <- 0.05 ; n <- 100
l.H <- qnorm(1 - alpha, lambda.0, sqrt(lambda.0 / n))
power <- 1 - pnorm(l.H, lambda.A, sqrt(lambda.A/n))
# openg()
plot(lambda.A, power, type = 'l', 
  xlab = expression(italic(lambda[A])))
# saveg('Poisson-power-large-sample-CPUE')
round(c(lambda.0 = lambda.0, n = n, 
  lambda.A = lambda.A[51], alpha = alpha, 
  power = power[51]), 3)
  
beta <- 0.2
n <- ((qnorm(1 - alpha / 2) * sqrt(lambda.0) -
  qnorm(beta) * sqrt(lambda.A)) / (lambda.0 - lambda.A))^2

openg()
plot(lambda.A, n, type = 'l', xlim = c(0.36, 0.42),
  ylim = c(0, 50000), ylog = TRUE)
saveg('Poisson-n-large-sample-CPUE')  
round(c(lambda.0 = lambda.0, n = ceiling(n[51]), 
  lambda.A = lambda.A[51], alpha = alpha, 
  power = 1 - beta), 3)

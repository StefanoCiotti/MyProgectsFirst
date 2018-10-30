rm(list = ls())

n <- 10000 ; alpha <- 0.01 ; mu.0 <- 100 ; 
X.bar <- 100.5 ; S <- 20 ; SE <- S / sqrt(n)
round(c(mu.0 = mu.0, x.H = qnorm(1 - alpha, mu.0, SE), 
  X.bar = X.bar, alpha = alpha), 2)

rm(list = ls())
mu.0 <- 3.1 ; X.bar <- 4.0 ; n <- 26 ; S <- sqrt(1.6)
SE <- S / sqrt(n) ; alpha <- 0.05
x.H <- mu.0 + qt(1 - alpha, mu.0, SE) * SE
round(c(mu.0 = mu.0, x.H = x.H, 
  X.bar = X.bar, alpha = alpha), 2)
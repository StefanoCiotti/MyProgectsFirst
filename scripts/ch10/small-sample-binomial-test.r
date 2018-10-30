rm(list = ls())
n <- 40 ; n.S <- 20 ; p <- 0.45 ; 
PI.0 <- n.S / n ; a <- 0.1
x <- binconf(n.S, n, method = 'wilson', alpha = a)
round(c(PI.0 = PI.0, x.L = x[2], p = p, alpha = a/2), 2)
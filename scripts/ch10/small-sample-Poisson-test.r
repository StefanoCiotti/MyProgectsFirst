rm(list = ls())
lambda.0 <- 6.5 ; l <- 3 ; alpha <- 0.05
round(c(lambda.0 = lambda.0, 
  x.L = qchisq(alpha, 2 * lambda.0) / 2, l = l, 
  alpha = alpha), 2)

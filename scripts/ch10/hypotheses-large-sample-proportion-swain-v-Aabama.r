rm(list = ls())

# lower-tailed

n <- 1050 ; n.S <- 177 ; PI.0 <- 0.25 ; alpha <- 0.01
SE <- sqrt(PI.0 * (1 - PI.0) / n)
round(c(PI.0 = PI.0, x.L = qnorm(alpha, PI.0, SE), 
  p = n.S / n, alpha = alpha), 2)

## upper-tailed
#
#set.seed(5) ; n <- 40 ; alpha <- 0.05
#X <- sample(age, n)
#mu.0 <- mean(age[cp$Race == 'Black'], na.rm = TRUE)
#X.bar <- mean(X) ; S <- sd(X) ; SE <- S / sqrt(n)
#round(c(mu.0 = mu.0, x.H = qnorm(1 - alpha, mu.0, SE), 
#  X.bar = X.bar, alpha = alpha), 2)
#  
## two-tailed
#
#set.seed(33) ; n <- 40 ; alpha <- 0.05
#X <- sample(age[cp$Sex == 'F'], n)
#mu.0 <- mean(age, na.rm = TRUE)
#X.bar <- mean(X) ; S <- sd(X) ; SE <- S / sqrt(n)
#round(c(mu.0 = mu.0, x.L = qnorm(alpha/2, mu.0, SE), 
#  x.H = qnorm(1 - alpha/2, mu.0, SE), X.bar = X.bar, 
#  alpha = alpha), 2)
#
rm(list = ls())

# lower-tailed

n <- 1 ; lambda.0 <- 25 ; l <- 30 ; alpha <- 0.05
SE <- sqrt(lambda.0 / n)
round(c(lambda.0 = lambda.0, 
  x.H = qnorm(1 - alpha, lambda.0, SE), 
  l = l, alpha = alpha), 2)

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
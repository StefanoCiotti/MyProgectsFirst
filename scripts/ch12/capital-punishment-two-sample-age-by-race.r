load('capital.punishment.rda')
cp <- capital.punishment

# age at sentencing
age <- cp[, 25] + cp[, 26] * 12 - cp[, 11] - cp[, 12] * 12
age <- age / 12

na.data <- is.na(cp[, 9]) | is.na(cp[, 11]) | is.na(cp[, 12]) |
   is.na(cp[, 23]) | is.na(cp[, 24])
w <- cp$Race == 'White' & !na.data
b <- cp$Race == 'Black' & !na.data

# age at sentencing, whites and blacks
x.1 <- age[w] ; N.1 <- length(x.1)
x.2 <- age[b] ; N.2 <- length(x.2)

mu.1 <- mean(x.1) ; mu.2 <- mean(x.2)
sigma.1 <- sd(x.1) ; sigma.2 <- sd(x.2)

set.seed(1)
n.1 <- 35 ; n.2 <- 40
X.1 <- sample(x.1, n.1) ; X.2 <- sample(x.2, n.2)
X.bar.1 <- mean(X.1) ; X.bar.2 <- mean(X.2)
var.1 <- var(X.1) ; var.2 <- var(X.2)
info <- rbind(mean = c(whites = X.bar.1, blacks = X.bar.2), 
   'variance' = c(var.1, var.2),
   'sample size' = c(n.1, n.2))
round(info, 1)

sd <- sqrt(var.1 / n.1 + var.2 / n.2)
1-pnorm(X.bar.1 - X.bar.2, 0, sd)
Z <- (X.bar.1 - X.bar.2) / sd ; Z
1-pnorm(Z)
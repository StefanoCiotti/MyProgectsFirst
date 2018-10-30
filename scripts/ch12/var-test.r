rm(list = ls())
set.seed(28) ; n.1 <- 20 ; n.2 <- 25
X.1 <- rnorm(n.1) ; X.2 = rnorm(n.2, 0, 2)
alpha <- 0.05 ; x <- var(X.1) / var(X.2)
p.L <- pf(x, n.1 - 1, n.2 - 1)

p.H <- 1 - pf(x, n.1 - 1, n.2 - 1)

p <- 2 * min(p.L, p.H)

p.value <- rbind(p.L, p.H, p)
dimnames(p.value) <- list(c('lower-tailed', 'upper-tailed',
  'two-tailed'), 'p-value') ; round(p.value, 3)

CI.L <- c(0, x / qf(alpha, n.1 - 1, n.2 - 1))
CI.H <- c(x / qf(1 - alpha, n.1 - 1, n.2 - 1), Inf)
CI <- c(x / qf(1 - alpha / 2, n.1 - 1, n.2 - 1),
  x / qf(alpha / 2, n.1 - 1, n.2 - 1))
CI <- rbind(CI.L, CI.H, CI)
dimnames(CI) <- list(c('lower-tailed', 'upper-tailed',
  'two-tailed'), c('low', 'high')) ; round(CI, 3)

var.test(X.1, X.2)


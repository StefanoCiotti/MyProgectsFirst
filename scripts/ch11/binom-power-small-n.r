rm(list = ls())
pi.0 <- 0.4 ; pi.A <- 0.5 ; alpha <- 0.05 ; n <- 25
#nu.1 <- 2 * (n - n.S + 1) ; nu.2 <- 2 * n.S
#nu.p.1 <- 2 * n.S + 2 ; nu.p.2 <- 2 * (n - n.S)
#
# test with binconf
#(p.L <- n.S / (n.S + qf(1 - alpha / 2, nu.1, nu.2) * (n - n.S + 1)))
#(p.H <- (n.S + 1) * qf(1 - alpha / 2, nu.p.1, nu.p.2) / 
#  (n - n.S + (n.S + 1) * qf(1 - alpha / 2, nu.p.1, nu.p.2)))
#
#binconf(n.S, n, alpha = alpha, method = 'all')
#

# binom.power
library(binom)
methods <- c("cloglog", "logit", "probit", "asymp", 
  "lrt", "exact")
results <- matrix(ncol = 1, nrow = length(methods))
for(i in 1 : length(methods)){
  results[i, 1] <- binom.power(pi.A, n = n, p = pi.0, 
    alpha = 0.05, alternative = 'greater', method = methods[i])
}
dimnames(results) <- list(methods, 'Power')
round(results, 3)

openg(4.5, 2.5)
tkbinom.power()
saveg('binom-power-small-n', 4.5, 2.5)
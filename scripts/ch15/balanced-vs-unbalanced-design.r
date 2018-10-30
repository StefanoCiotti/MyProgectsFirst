rm(list = ls())

generate.data <- function(means, SD, n){
  lev <- c('A', 'B', 'C')
  set.seed(1)
  d <- round(c(rnorm(n[1], means[1], SD),
    rnorm(n[2], means[2], SD),
    rnorm(n[3], means[3], SD)), 2)
  d <- data.frame(factor = c(rep(lev[1], n[1]),
    rep(lev[2], n[2]), rep(lev[3], n[3])),
    value = d)
  (m <- tapply(d$value, d$factor, mean))
  d <- cbind(d, m = c(rep(m[1], n[1]),
    rep(m[2], n[2]), rep(m[3], n[3])))

  Total.mean <- mean(d$value)
  Within.MS <- sum(tapply((d$value - d$m)^2,
    d$factor, sum)) / (sum(n) - length(n))
  Between.MS <- sum(tapply((d$m - Total.mean)^2,
   d$factor, sum)) / (length(n) - 1)
  F <- Between.MS / Within.MS
  p.value <- 1 - pf(F, length(n) - 1, sum(n) - length(n))
  x <- round(rbind(c(Within.MS = Within.MS, Between.MS = Between.MS, F = F,
    p.value = p.value),
  c(n = c(n, NA))), 2)
  dimnames(x)[[1]] <- c('SS', 'n')
  x
}

means <- c(10, 20, 30) ; SD <- 15
n.1 <- c(10, 10, 10)
n.2 <- c(5, 15, 10)
n.3 <- c(15, 10, 5)
print(generate.data(means, SD, n.1))
print(generate.data(means, SD, n.2))
print(generate.data(means, SD, n.3))


















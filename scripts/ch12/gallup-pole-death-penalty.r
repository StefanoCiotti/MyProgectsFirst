rm(list = ls())
n <- c(500, 500) ; p <- c(0.59, 0.71) ; alpha <- 0.05
p.bar <- sum(n * p) / sum(n) 
SE <- sqrt(p.bar * (1 - p.bar) * sum(1 / n))
pi.hat <- p[2] - p[1]
round(c(low = qnorm(alpha / 2, pi.hat, SE),
  high = qnorm(1 - alpha / 2, pi.hat, SE)), 2)

options(width = 60)
correction <- prop.test(n * p, n)
no <- prop.test(n * p, n, correct = FALSE)
names(no)
CI <- rbind(correction$conf.int, no$conf.int)
dimnames(CI) <- list(c('correction', 'no'),
  c('low', 'high'))
round(CI, 2)

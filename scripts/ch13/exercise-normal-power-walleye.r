rm(list = ls())
source('power-normal.R')

load('walleye.rda')
alpha <- 0.05 ; mu.0 <- 0 ; mu.1 <- mean(walleye[[1]])
mu.2 <- mean(walleye[[2]]) ;
mu.A <- seq(-1, 1, length = 201)
V.1 <- var(walleye[[1]]) ; V.2 <- var(walleye[[2]])
n.1 <- length(walleye[[1]]) ; n.2 <- length(walleye[[2]])

par(mfrow = c(1, 3))
alt <- c('two.sided', 'greater', 'less')
for (i in 1 : 3){
  if(i == 1) ylab = 'power' else ylab = ''
  p <- power.normal(mu.A = mu.A, mu.0 = mu.0, n.1 = n.1,
    n.2 = n.2, S.1 = sqrt(V.1), S.2 = sqrt(V.2),
    alt = alt[i])
  plot(p$pwr, xlab = expression(mu[A]),
    ylab = ylab, type = 'l', main = alt[i])
}

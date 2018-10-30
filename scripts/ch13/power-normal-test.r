source('power-normal.R')

alpha <- 0.05 ; mu.0 <- 0 ; mu.1 <- 29 ; mu.2 <- 27.6
mu.A <- seq(-10, 10, length = 201) ; V.1 <- 64.8
V.2 <- 61.1 ; n.1 <- 35 ; n.2 <- 40 ; k <- n.2 / n.1

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
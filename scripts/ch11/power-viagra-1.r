rm(list = ls())
mu.0 <- 18 ; S <- 1.23 ; n <- 48 ; SE <- S / sqrt(n)
X.bar <- 18.52 ; alpha <- 0.05
round(c(mu.0 = mu.0, x.H = qnorm(1 - alpha, mu.0, SE), 
  X.bar = X.bar, alpha = alpha, 
  p.value = 1 - pnorm(X.bar, mu.0, SE)), 3)

x <- seq(mu.0 - 4 * SE, mu.0 + 4 *SE, length = 501)
openg()
plot(x, dnorm(x, mu.0, SE), axes = F, type = 'l', 
  xlab = '', ylab = '')
abline(h = 0)
axis(1, at = c(mu.0, 
  round(qnorm(1 - alpha, mu.0, SE), digits = 1)), 
  labels = c(mu.0, 
  round(qnorm(1 - alpha, mu.0, SE), digits = 1)))
x1 <- x[x>= qnorm(1 - alpha, mu.0, SE)]
y1 <- dnorm(x1, mu.0, SE)
x.poly <- c(qnorm(1 - alpha, mu.0, SE), x1, mu.0 + 
   4 * SE, qnorm(1 - alpha, mu.0, SE))
y.poly <- c(0, y1, 0, 0)
polygon(x.poly, y.poly, col = 'gray90')
saveg('power-viagra-1')
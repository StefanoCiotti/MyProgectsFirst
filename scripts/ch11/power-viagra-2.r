rm(list = ls())
mu.0 <- 18 ; S <- 1.23 ; n <- 48 ; SE <- S / sqrt(n)
X.bar <- 18.52 ; alpha <- 0.05

mu.A <- c(18.1, 18.2, 18.3, 18.4)
x.H <- qnorm(1 - alpha, mu.0, SE)
beta <- matrix(nrow = 2, ncol = length(mu.A))
beta[1, ] <- mu.A
beta[2, ] <- round(pnorm(x.H, mu.A, SE), 2)
dimnames(beta) <- list(c('mu.A', 'beta'), 
  rep('',length(mu.A))) ; beta


openg(4.5, 4.5)
par(mfrow = c(2, 2))
titles <- c('A', 'B', 'C', 'D')
x <- seq(mu.0 - 4 * SE, mu.0 + 4 * SE, length = 501)
for (i in 1 : length(mu.A)){
   plot(x, dnorm(x, mu.0, SE), axes = FALSE, type = 'l', 
    xlab = '', ylab = '')
   abline(h = 0)
   ticks <- c(mu.0, mu.A[i], round(qnorm(1 - alpha, mu.0, SE), digits = 1))
   if(i == 3) muAA <- 18.3
   else muAA <- expression(italic(mu[A]))
   tick.labels <- c( 
      expression(italic(mu[0])), muAA, 
      round(qnorm(1 - alpha, mu.0, SE), digits = 1))
   axis(1, at = ticks, labels = tick.labels, las = 2)
   x1 <- x[x >= qnorm(1 - alpha, mu.0, SE)]
   y1 <- dnorm(x1, mu.0, SE)
   x2 <- c(qnorm(1 - alpha, mu.0, SE), x1, mu.0 + 4 * SE, 
    qnorm(1 - alpha, mu.0, SE))
   y2 <- c(0, y1, 0, 0)
   polygon(x2, y2, col = 'gray90')

   lines(x, dnorm(x, mu.A[i], SE))
   x1 <- x[x <= qnorm(1 - alpha, mu.0, SE)]
   y1 <- dnorm(x1, mu.A[i], SE)
   x.poly <- c(mu.0 - 4 * SE, x1, qnorm(1 - alpha , mu.0, SE), 
    qnorm(1 - alpha, mu.0, SE))
   y.poly <- c(0, y1, 0, 0)
   polygon(x.poly, y.poly, col = 'gray80')
   lines(x, dnorm(x, mu.0, SE))
   abline(v = mu.0, lty = 2) ; abline(v = mu.A[i], lty = 2)
   title(titles[i])
}
saveg('power-viagra-2', 4.5, 4.5)
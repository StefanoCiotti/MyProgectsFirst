rm(list = ls())
lambda.0 <- 10 ; alpha <- 0.05 ; lambda.A <- c(9, 11)
l.L <- qchisq(alpha, 2 * lambda.0) / 2
l.H <- qchisq(1 - alpha, 2 * (lambda.0 + 1)) / 2
round(c('lower-tailed' =
  pchisq(l.L, 2 * lambda.A[1]) / 2,
  'upper-tailed' =
  1 - pchisq(l.H, 2 * (lambda.A[2] + 1)) / 2), 3)

min.x <- c(2,0) ; max.x <- c(6, 60)
min.y <- c(0,0) ; max.y <- c(0.0015, 0.032)

openg(4.5, 4.5)
par(mfrow = c(2, 2))
all.x <- seq(4, 60, length = 501)

plot(all.x, dchisq(all.x, 2 * lambda.A[1]) / 2, type = 'l', 
  axes = FALSE, main = 'A', xlab = '', ylab = '', lty = 2)
lines(all.x, dchisq(all.x, 2 * lambda.0) / 2)
abline(h = 0)
axis(1, pos = 0, at = c(l.L, lambda.0), 
  labels = c(expression(italic(l[L])),
  expression(italic(lambda[0]))))

x <- seq(min.x[1], max.x[1], length = 501)
y.limits <- c(min.y[1], max.y[1])
x.limits <- c(min.x[1], max.x[1])
plot(x, dchisq(x, 2 * lambda.0) / 2, type = 'l', 
  axes = FALSE, main = 'B', xlab = '', ylab = '', 
  xlim = x.limits, ylim = y.limits)
abline(h = 0)
x.poly <- c(min.x[1], x[x < l.L], l.L, l.L, 0)
y.poly <- c(0, dchisq(x[x <= l.L], 2 * lambda.0) / 2,  
  dchisq(l.L, 2 * lambda.0) / 2, 0, 0)
polygon(x.poly, y.poly, col = 'gray90')
x.poly <- c(min.x[1], x[x < l.L], l.L, l.L, 0)
y.poly <- c(0, dchisq(x[x <= l.L], 2 * lambda.A[1]) / 2,  
  dchisq(l.L, 2 * lambda.A[1]) / 2, 0, 0)
polygon(x.poly, y.poly, col = 'gray60')
lines(x, dchisq(x, 2 * lambda.A[1]) / 2, lty = 2)
lines(x, dchisq(x, 2 * lambda.0) / 2)
axis(1, pos = 0, at = c(l.L, lambda.0), 
  labels = c(expression(italic(l[L])),
  expression(italic(lambda[0]))))




plot(all.x, dchisq(all.x, 2 * (lambda.0 + 1)) / 2, type = 'l', 
  axes = FALSE, main = 'C', xlab = '', ylab = '')
lines(all.x, dchisq(all.x, 2 * (lambda.A[2] + 1)) / 2, lty = 2)  
abline(h = 0)
axis(1, pos = 0, at = c(lambda.0, l.H), 
  labels = c(expression(italic(lambda[0])),
  expression(italic(l[H]))))

x <- seq(min.x[2], max.x[2], length = 501)
y.limits <- c(min.y[2], max.y[2])
x.limits <- c(min.x[2], max.x[2])
plot(x, dchisq(x, 2 * (lambda.0 + 1)) / 2, type = 'l', 
  axes = FALSE, main = 'D', xlab = '', ylab = '', 
  xlim = x.limits, ylim = y.limits)
abline(h = 0)
x.poly <- c(l.H, l.H, x[x > l.H], max.x[2], max.x[2], l.H)
y.poly <- c(0, dchisq(l.H, 2 * (lambda.0 + 1)) / 2, 
  dchisq(x[x > l.H], 2 * (lambda.0 + 1)) / 2,  
  dchisq(max.x[2], 2 * (lambda.0 + 1)) / 2, 0, 0)
polygon(x.poly, y.poly, col = 'gray90')
y.poly <- c(0, dchisq(l.H, 2 * (lambda.A[2] + 1)) / 2, 
  dchisq(x[x > l.H], 2 * (lambda.A[2] + 1)) / 2,  
  dchisq(max.x[2], 2 * (lambda.A[2] + 1)) / 2, 0, 0)
polygon(x.poly, y.poly, col = 'gray60')
lines(x, dchisq(x, 2 * (lambda.A[2] + 1)) / 2, lty = 2)
lines(x, dchisq(x, 2 * (lambda.0 + 1)) / 2)
axis(1, pos = 0, at = c(lambda.0, l.H), 
  labels = c(expression(italic(lambda[0])),
  expression(italic(l[H]))))
saveg('Poisson-power-small-sample', 4.5, 4.5)
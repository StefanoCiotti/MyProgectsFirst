rm(list = ls())
mu <- 71 ; S <- 2.9 ; n <- 45 ; SE <- S / sqrt(n)
alpha <- 0.2
y.limit <- c(0, dnorm(mu, mu, SE))
x <- seq( mu - 4 * SE, mu + 4 * SE, length = 501)
y <- dnorm(x, mu, SE) ; dy <- 0.05

# lower tailed

openg()
x.L <- qnorm(alpha, mu, SE)
plot(x, y, axes = F, type = 'l', 
   ylab = '', xlab = 'lower-tailed')
abline(h = 0)
x.poly <- c(x.L, x[x >= x.L], max(x), x.L) ; 
y.poly <- c(0, dnorm(x[x >= x.L], mu, SE), 0, 0)
polygon(x.poly, y.poly, col = 'gray90')
axis(1, pos = 0, at = c( x.L, mu), 
   labels = c(expression(italic(x[L])), 
   expression(italic(mu[0]))))
text(mu, 3 * dy, pos = 4, expression(italic(1 - alpha)))
text(x.L, dy, pos = 2,
  expression(italic(alpha)))
saveg('large-sample-mean-sparrows-lower-tailed')

# two tailed

openg()
x.H <- qnorm(1 - alpha / 2, mu, SE)
x.L <- qnorm(alpha / 2, mu, SE)
plot(x, y, axes = F, type = 'l', 
   ylab = '', xlab = 'two-tailed')
abline(h = 0)
x.poly <- c(x.L, x[x >= x.L & x <= x.H], x.H, x.L)
y.poly <- c(0, dnorm(x[x >= x.L & x <= x.H], mu, SE), 0, 0)
polygon(x.poly, y.poly, col = 'gray90')
axis(1, pos = 0, at = c(x.L, mu, x.H), 
   labels = c(expression(italic(x[L])), 
   expression(italic(mu[0])), 
   expression(italic(x[H]))))
text(mu, 3 * dy, expression(italic(1 - alpha)))
text(x.H, dy, pos = 4, 
  expression(italic(over(alpha, 2))))
text(x.L, dy, pos = 2, 
  expression(italic(over(alpha, 2))))
saveg('large-sample-mean-sparrows-two-tailed')

# upper tailed

openg()
x.H <- qnorm(1 - alpha, mu, SE)
plot(x, y, axes = F, type = 'l', 
   ylab = '', xlab = 'upper-tailed')
abline(h = 0)
x.poly <- c(min(x), x[x <= x.H] , x.H, min(x))
y.poly <- c(0, dnorm(x[x <= x.H], mu, SE), 0, 0)
polygon(x.poly, y.poly, col = 'gray90')
axis(1, pos = 0, at = c(mu, x.H), 
   labels = c(expression(italic(mu[0])), 
    expression(italic(x[H]))))
text(mu, 3 * dy, pos = 2, expression(italic(1 - alpha)))
text(x.H, dy, pos = 4, 
  expression(italic(alpha)))
saveg('large-sample-mean-sparrows-upper-tailed')

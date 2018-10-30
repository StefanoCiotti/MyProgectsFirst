par(mfrow = c(2, 2))
xlim=c(-10, 20) ; ylim = c(0, 0.4)
hist(X.1, xlim = xlim, main = '', freq = FALSE, 
   xlab = 'X.1', ylab = 'density', ylim = ylim)
hist(X.2, xlim = xlim, main = '', freq = FALSE, 
   xlab = 'X.2', ylab = '', ylim = ylim)
hist(X.2-X.1, xlim = xlim, main = '', freq = FALSE, 
   xlab = 'X.2 - X.1', ylab = 'density', ylim = ylim)
x <-seq(-10, 20, length = 201)
plot(x, dnorm(x, 10, 1), xlab = 'X.1, X.2, X.2 - X.1',
   ylab = '', type = 'l', ylim = ylim)
lines(x, dnorm(x, 12, 2))
lines(x, dnorm(x, 12 - 10, 1 + 2), lwd = 2)
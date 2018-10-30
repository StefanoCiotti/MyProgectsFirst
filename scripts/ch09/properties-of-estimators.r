openg(4.5, 2.5, pointsize = 14)
par(mfrow  = c(1,3), mar = c(2, 1, 2, 1))

x <- seq(-4, 4, length = 1000)
plot(x, dnorm(x), axes = FALSE, 
   xlab = expression(plain('(a)')), 
   ylab = '', type = 'l')
abline(h = 0) ; abline(v = 0)
axis(1, at = -1, 
   labels = expression(italic('parameter')),
   lwd = 3)
axis(3, at = 0, labels = expression(italic('estimator')))

x <- seq(-3, 3, length = 1000)
plot(x, dnorm(x, 0, 1.5), axes = F,
   xlab = expression(plain('(b)')), 
   ylab = '', type = 'l')
abline(h = 0.035) ; abline(v = 0)
axis(1, at = 0, 
   labels = expression(italic('parameter')), 
   lwd = 3)
axis(3, at = 0, 
   labels = expression(italic('estimator')))

x <- seq(-3, 3, length = 1000)
plot(x, dnorm(x, 0, 0.25), axes = F, 
   xlab = expression(plain('(c)')), ylab = '', type = 'l')
abline(h = 0) ; abline(v = 0)
axis(1, at = 0, 
   labels = expression(italic('parameter')), 
   lwd = 3)
axis(3, at = 0, labels = expression(italic('estimator')))

saveg('properties-of-estimators', 4.5, 2.5)

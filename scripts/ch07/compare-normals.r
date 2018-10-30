x <- seq(-3, 5, length = 501)
plot(x, dnorm(x), type = 'l', ylim = c(0, 1), xlab = 'x',
   ylab = 'density')
lines(x, dnorm(x, 2, .5))
text(0, .44, expression(paste(italic(mu[X]) == 0, ',  ',
   italic(sigma[X]) == 1)))
text(2, .84, expression(paste(italic(mu[Y]) == 2, ',  ',
   italic(sigma[Y]) == 0.5)))
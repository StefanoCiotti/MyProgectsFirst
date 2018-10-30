rm(list = ls())
x <- 0 : 60 ; lambda <- c(2, 10, 25, 35)
y <- seq(0, 40, length = 501)
xl <- expression(italic(x)) ; xlabel <- c('', '', xl, xl)
yl <- expression(italic(P(X==x)))
ylabel <- c(yl, '', yl, '')
xlimit <- rbind(c(0, 10), c(0, 20), c(5, 45), c(15, 60))
par(mfrow = c(2, 2))
for(i in 1 : 4){
  plot(x, dpois(x, lambda[i]), type = 'h', xlab = xlabel[i],
  ylab = ylabel[i], xlim = xlimit[i, ]) ; abline(h = 0)
  points(x, dpois(x, lambda[i]), pch = 19, cex = 1.5)
  lines(y, dnorm(y, lambda[i], sqrt(lambda[i])))
}
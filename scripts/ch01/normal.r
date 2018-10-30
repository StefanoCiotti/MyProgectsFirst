x <- seq(-3, 3, length = 501)
plot(x, dnorm(x), axes = FALSE, type = 'l', xlab = 'Normal Curve',  ylab = '') ; 
abline(h = 0)
x <- 0 ; 
lines(c(0, 0), c(dnorm(x), -0.01))
x <-  -1 ; 
lines(c(-1, 0), c(dnorm(x), dnorm(x)))
arrows(-1, dnorm(x), 0, dnorm(x), code = 3, length = 0.1)
text(0.2, 0.2, expression(italic(mu)))
text(-0.5, 0.26, expression(italic(sigma)))



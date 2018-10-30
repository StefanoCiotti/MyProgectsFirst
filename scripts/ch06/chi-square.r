x <- seq(0, 35, length = 101)
nu <- c(5, 10, 15)
ylabel <- c('dchisq(x, nu)', 'pchisq(x, nu)')
par(mfrow = c(1, 2))
plot(x, dchisq(x, nu[1]), type = 'l', ylab = ylabel[1])
lines(x, dchisq(x, nu[2]), lty = 2)
lines(x, dchisq(x, nu[3]), lty = 3)
text(locator(), labels = c('nu = 5', 'nu = 10', 'nu = 15'),
  pos = 4)
plot(x, pchisq(x, nu[1]), type = 'l', ylab = ylabel[2])
lines(x, pchisq(x, nu[2]), lty = 2)
lines(x, pchisq(x, nu[3]), lty = 3)
text(locator(), labels = c('nu = 5', 'nu = 10', 'nu = 15'),
  pos = 4)
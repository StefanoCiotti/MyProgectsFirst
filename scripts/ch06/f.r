x <- seq(-1, 5, length = 101)
nu.1 <- c(1, 10) ; nu.2 <- 1
ylabel <- c('df(x, nu)', 'pf(x, nu)')
par(mfrow = c(1, 2))
plot(x, df(x, nu.1[1], nu.2), type = 'l', ylab = ylabel[1])
lines(x, df(x, nu.1[2], nu.2), lty = 2)
text(locator(), labels = c('nu.1 = 1', 'nu.1 = 10'),
  pos = 4)
plot(x, pf(x, nu.1[1], nu.2), type = 'l', ylab = ylabel[2])
lines(x, pf(x, nu.1[2], nu.2), lty = 2)
text(locator(), labels = c('nu.1 = 1', 'nu.1 = 10'),
  pos = 4)
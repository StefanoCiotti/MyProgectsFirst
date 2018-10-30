n <- 1000 ; set.seed(1000) ; df <- 10 ; X <- rchisq(n, df)
#openg()
h(X, ylim = c(0, 0.1), xlab = 'x')
x <- seq(0, 40, length = 101)
lines(x, dchisq(x, df), lwd = 2, col = 'red')

df.hat <- fitdistr(X, densfun = 'chi-squared',
  start = list(df = 5))
lines(x, dchisq(x, df.hat[[1]]), lwd = 2,
  lty = 2, col = 'blue')
#saveg('chi-squared-fitting')
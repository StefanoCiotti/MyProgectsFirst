a <- 13 ; b <- 15 ; x <- seq( -1, b + 1, length = 2001)
par(mfrow = c(2, 2))
xlabel <- c('', '', rep(expression(italic(t)), 2))
ylabel <- c('', expression(italic(P(T < t))), '',
  expression(italic(P(T==t))))
y <- cbind(punif(x), punif(x, a, b),
  dunif(x), dunif(x, a, b))
x.limits <- rbind(c(-0.1, 1.1), c(a - 0.1, b + 0.1),
  c(-0.1, 1.1), c(a - 0.1, b + 0.1))
for(i in c(2, 1, 4, 3)){
  plot(x, y[, i], type = 'l', xlab = xlabel[i],
    xlim = x.limits[i, ], ylab = ylabel[i],
    ylim = c(0, 1.1), lwd = 2)
}
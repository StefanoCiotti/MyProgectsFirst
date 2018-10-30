par(mfrow = c(1, 2)) ; n <- c(20, 4) ; PI <- c(0.4, 0.04)
ylimits <- rbind(c(0, 0.2), c(0, 1.0))
ylabel <- c(expression(italic(P(X == x))), '')
for(i in 1 : 2){
  xy <- list(cbind(0 : n[i], dbinom(0 : n[i], n[i], PI[i])),
    seq(0, n[i], length = 1001))
  plot(xy[[1]], type = 'h', lwd = 2, ylim = ylimits[i, ],
    xlab = expression(italic(x)), ylab = ylabel[i])
  lines(xy[[2]], dnorm(xy[[2]], n[i] * PI[i],
    sqrt(n[i] * PI[i] * (1 - PI[i]))))
  abline(h = 0, lwd = 2)
}
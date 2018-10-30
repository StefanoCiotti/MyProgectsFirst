rm(list = ls())
par(mfrow = c(2, 2)) ; PI <- 0.508 ; n <- c(7, 40)
sigma <- sqrt(PI * (1 - PI) / n)
R <- 5000 ; x <- seq(0, 1, length = 201)
xlabel.1 <- expression(italic(x))
xlabel.2 <- c(expression(italic(paste(n == 7))),
  expression(italic(paste(n == 40))))
ylabel <- c(expression(italic(paste(P(X<=x), ', n = ', 7))),
  expression(italic(paste(P(X<=x), ', n = ', 40))))
set.seed(7)
for (i in 1 : 2){
  plot(1 : n[i], dbinom(1 : n[i], n[i], PI), type = 'h',
    lwd = 2, xlab = xlabel.1, ylab = ylabel[i])
  abline(h = 0, lwd = 2)
  p <- rbinom(R, n[i], PI) / n[i]
  h(p, xlim = c(0, 1), ylim = c(0, 5), axes = FALSE,
    xlab = xlabel.2[i])
    axis(1, at = c(0, PI, 1), las = 2) ; axis(2)
  lines(x, dnorm(x, PI, sigma[i]))
}
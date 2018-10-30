mu <- 18 ; sigma <- 6
boundary <- c(mu - 3 * sigma, mu + 3 * sigma)
x <- seq(boundary[1], boundary[2], length = 1001)
y <- dnorm(x, mu, sigma)
plot(x, y, type='l', xlab = expression(italic(x)),
  ylab = expression(italic(P(X==x))))
abline(h = 0)
x1 <- x[x <= 20.5] ; y1 <- dnorm(x1, mu, sigma)
x2 <- c(boundary[1], x1, x1[length(x1)], boundary[2])
y2 <- c(0, y1, 0, 0)
polygon(x2, y2, col = 'grey80')
x1 <- x[x <= 19.5]
y1 <- dnorm(x1, mu, sigma)
x2 <- c(boundary[1], x1, x1[length(x1)], boundary[2])
y2 <- c(0, y1, 0, 0)
polygon(x2, y2, col = 'grey90')
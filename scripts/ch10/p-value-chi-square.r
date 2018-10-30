rm(list = ls())
lambda.0 <- 6.5 ; l <- 2.5 ; alpha <- 0.1
round(c(lambda.0 = lambda.0, 
  x.L = qchisq(alpha / 2, 2 * lambda.0) / 2, 
  x.H = qchisq(1 - alpha / 2, 2 * (lambda.0 + 1)) / 2, l = l,
  alpha = alpha / 2), 3)
  
(lower.p <- pchisq(2 * l, 2 * lambda.0))
(upper.p <- 1 - pchisq(2 * l, 2 * (lambda.0 + 1)))
p.value <- min(lower.p, upper.p)
round(c(lambda.0 = lambda.0, l = l, alpha = alpha / 2,
  p.value = p.value), 3)  


x <- seq(0, 40, length = 201)

# lower-tailed density
# openg(4.5, 2.5)
par(mfrow = c(1, 2))
plot(x, dchisq(x, 2 * lambda.0) / 2, type = 'l', ylab = 'density', axes = FALSE)
axis(2)
abline(h = 0)

# lower significance polygon
x.low <- seq(0, qchisq(alpha / 2, 2 * lambda.0) / 2, length = 201)
x.poly <- c(0, x.low, x.low[201], 0)
y.low <- dchisq(x.low, 2 * lambda.0) / 2
y.poly <- c(0, y.low, 0, 0)
polygon(x.poly, y.poly, col = 'gray90')

# p-value polygon
x.low <- seq(0, l, length = 201)
x.poly <- c(0, x.low, x.low[201], 0)
y.low <- dchisq(x.low, 2 * lambda.0) / 2
y.poly <- c(0, y.low, 0, 0)
polygon(x.poly, y.poly, col = 'black')

axis(1, pos = 0, at = c(0, l, 10, 20, 30, 40), 
  labels = c(0, expression(italic(l[0])), 10, 20, 30, 40))

# upper significance polygon
x.low <- seq(qchisq(1 - alpha / 2, 2 * (lambda.0 + 1)) / 2, 60, length = 201)
x.poly <- c(x.low[1], x.low[1], x.low, x.low[1])
y.low <- dchisq(x.low, 2 * (lambda.0 + 1)) / 2
y.poly <- c(0, y.low[1], y.low, 0)
polygon(x.poly, y.poly, col = 'gray90')

lines(x, dchisq(x, 2 * (lambda.0 + 1)) / 2, lty = 2)


# second panel
plot(x, dchisq(x, 2 * lambda.0) / 2, type = 'l', ylab = '',
  xlim = c(1, 4.1), ylim = c(0, 0.0025), axes = FALSE)
axis(2)
abline(h = 0)
x.low <- seq(0, qchisq(alpha / 2, 2 * lambda.0) / 2, length = 201)
x.poly <- c(0, x.low, x.low[201], -0)
y.low <- dchisq(x.low, 2 * lambda.0) / 2
y.poly <- c(0, y.low, 0, 0)
polygon(x.poly, y.poly, col = 'gray90')

x.low <- seq(0, l, length = 201)
x.poly <- c(0, x.low, x.low[201], -0)
y.low <- dchisq(x.low, 2 * lambda.0) / 2
y.poly <- c(0, y.low, 0, 0)
polygon(x.poly, y.poly, col = 'black')
lines(x, dchisq(x, 2 * (lambda.0 + 1)) / 2, lty = 2)
axis(1, pos = 0, at = c(1, l, qchisq(alpha / 2, 2 * lambda.0) / 2, 4), 
  labels = c(1, expression(italic(l[0])), expression(italic(x[L])), 4))
# saveg('p-value-chi-square', 4.5, 2.5)

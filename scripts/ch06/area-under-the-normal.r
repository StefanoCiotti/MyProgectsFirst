x <- seq(-4, 4, length = 1000) ; y <- dnorm(x)
plot(x, y, axes = FALSE, type = 'l', xlab = '', ylab = '',
   main = expression(italic(P(X<=a))))
abline(h = 0)
x1 <- x[x <= -1] ; y1 <- dnorm(x1)
x2 <- c(-4, x1, x1[length(x1)], -4) ; y2 <- c(0, y1, 0, 0)
polygon(x2, y2, col = 'grey90')
axis(1, at = c(-1, 1), font = 8, 
   vfont = c('serif', 'italic'), labels = c('a', 'b'))
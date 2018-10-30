rm(list = ls())
k <- 5 ; n <- rep(20, k) ; repetitions <- 3
means <- rbind(seq(200, 208, by = 2), seq(200, 220, by = 5),
  seq(200, 232, by = 8))
SD <- 50 ; l <- LETTERS[1 : k]

treatment <- vector()
for(i in 1 : k) treatment <- c(treatment, rep(l[i], n[i]))
d <- data.frame(treatment)

set.seed(16)
for(i in 1 : repetitions){
  value <- vector()
  for(j in 1 : k)
    value <- c(value, rnorm(n[j], means[i, j], SD))
  d <- data.frame(d, value = round(value, 3))
}

openg(4, 4)
par(mfrow=c(2,2))
plot(d[,c(1,2)], ylim = c(0, 350))
plot(d[,c(1,3)], ylim = c(0, 350))
plot(d[,c(1,4)], ylim = c(0, 350))

a <- matrix(nrow = 3, ncol = 2)
for(i in 1 : length(means[, 1])){
  s <- summary(aov(d[, i + 1] ~ d[, 1], data = d))
  a[i, ] <- round(c('F' = s[[1]][[4]][1],
    'p-value' = s[[1]][[5]][1]), 3)
}
dimnames(a) <- list(c('small difference in means',
  'larger difference', 'largest difference'),
  c('F-ratio', 'p-value'))
a

x <- seq(0, 5, length = 201)
y <- df(x, k - 1, n - k)
plot(x, y, type = 'l', ylab = expression(italic(F)),
  xlab = expression(italic(x)))
q <- qf(0.95, k - 1, sum(n) - k)
xx <- seq(q, 10, length = 101)
yy <- df(xx, k - 1, n - k)
x.poly <- c(q, xx, q)
y.poly <- c(0, yy, 0)
polygon(x.poly, y.poly, col = 'gray90')
for(i in 1 : length(means[, 1])){
  x0 <- a[i, 1] ; y0 <- 0 ;
  x1 <- a[i, 1] ; y1 <- df(a[i, 1], k - 1, n - 1)
  segments(x0, y0, x1, y1)
}
segments(q, 0, q, df(q, k - 1, n - k), lwd = 2)
abline(h = 0)
text(locator(1), label = 'rejection region')
saveg('F-density-and-F-ratio', 4, 4)
a

j <- 10 ; n.samples <- 10000
i <- 3 ; i.largest <- vector()
set.seed(100)
for(k in 1 : n.samples){
  x <- sort(runif(i + j - 1), decreasing = TRUE)
  i.largest[k] <- x[i]
}
openg(4.75, 2.5)
par(mfrow = c(1, 2))
h(i.largest)
xx <- seq(0, 1, length = 101)
lines(xx, dbeta(xx, j, i ))
plot(ecdf(x), ylab = expression(italic(P(X<=x))), main = '',
  xlim = c(0, 2))
lines(xx, pbeta(xx, j, i))
saveg('beta', 4.75, 2.5)
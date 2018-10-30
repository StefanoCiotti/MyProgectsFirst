set.seed(100)
n.S <- ifelse(runif(1, 0, 1) < 0.25, 1, 0)
p <- vector()
for(n in 1 : 10000){
  n.S <- n.S + ifelse(runif(1, 0, 1) < 0.25, 1, 0)
  p[n] <- n.S / n
}
plot(p, type = 'l', ylim = c(0, 0.3),
  xlab = expression(italic(n)), ylab = expression(italic(p)))
abline(h = .25)

p <- cumsum(ifelse(runif(10000, 0, 1) < 0.25, 1, 0)) / (1:10000)

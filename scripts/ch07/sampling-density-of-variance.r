par(mfrow = c(1,2)) ; ylimits <- c(0, 1)
R <- 50000 ; n <- 20

s.loop <- function(R, n){
  set.seed(10)
  v <- vector()
  for(i in 1 : R) v[i] <- var(rexp(n)) ; v
}
fast <- system.time((v <- s.loop(R, n)))[1 : 3]
h(v, xlab = 'fast', ylim = ylimits) ; lines(density(v))

s <- function(R, n){
  set.seed(10)
  g.l <- gl(R, n); v <- rexp(R * n)
  mapply(var, split(v, g.l))
}
faster <- system.time((v <- s(R, n)))[1 : 3]
h(v, xlab = 'faster', ylab = '', ylim = ylimits)
lines(density(v))

s.1 <- function(R, n){
  set.seed(10)
  m <- matrix(rexp(R * n), nrow = n, ncol = R)
  apply(m, 2, var)
}

fastest <- system.time((v <- s.1(R, n)))[1 : 3]
h(v, xlab = 'fastest', ylab = '', ylim = ylimits)
lines(density(v))


cpu <- rbind(faster, fastest)
dimnames(cpu)[[2]] <- c('user', 'system', 'elapsed')
print(cpu)


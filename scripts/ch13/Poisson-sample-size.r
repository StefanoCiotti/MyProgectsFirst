Poisson.sample.size <- function(t, n, e, 
   rho = (e[1] / n[1]) / (e[2] / n[2]), alpha = 0.05, 
   power = 0.8, k = 1){
   q <- qnorm(1 - alpha / 2)
   p <- qnorm(power)
   p0 <- t[1] / sum(t) ; v0 <- p0 * (1 - p0)
   pa <- t[1] * rho / (t[1] * rho + t[2])
   va <- pa * (1 - pa)
   m <- (q * sqrt(v0) + p * sqrt(va)) / (abs(p0 - pa))
   m <- ceiling(m * m)
   d <- k + 1 - exp(-e[1] / n[1] * t[1]) - 
      k * exp(-e[2] / n[2] * t[2])
   n1 <- m / d; n2 <- k * n1
   ceiling(c(n1, n2))
}
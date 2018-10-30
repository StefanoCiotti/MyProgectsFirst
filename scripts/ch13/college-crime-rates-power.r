poisson.power <- function(t, n, l, alpha = 0.05){
  q <- qnorm(1 - alpha / 2)
  T <- t * n
  rho <- l[1] / l[2]
  p0 <- T[1] / sum(T) ; v0 <- p0 * (1 - p0)
  pa <- T[1] * rho / (T[1] * rho + T[2])
  va <- pa * (1 - pa)
  m <- sum(n * (1 - exp(-l * t)))
  A <- ((pa - p0) * sqrt(m) - q * sqrt(v0)) / sqrt(va)
  B <- ((p0 - pa) * sqrt(m) - q * sqrt(v0)) / sqrt(va)
  pnorm(A) + pnorm(B)
}

data(college.crime)
um <- college.crime[309, c(8, 64 : 70)]
uw <- college.crime[670, c(8, 64 : 70)]
l <- c(sum(um[2 : 8]) / um[, 1] , sum(uw[2 : 8]) / uw[, 1])
n <- c(1000, 1000) 
t <- c(4, 4)

p <- poisson.power(t, n, l)
print(p)
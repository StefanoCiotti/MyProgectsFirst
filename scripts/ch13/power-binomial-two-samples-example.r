rm(list = ls())
library(Hmisc)
source('bp.R')

n <- c(40, 40) ;  n.S <- c(10, 15) ; p <- n.S / n
Power <- bp(p[1], p[2], n1 = n[1], n2 = n[2],
  alt = 'greater')
print(c(beta = 1 - as.vector(Power)))

pi.A <- seq(0, p[2], length = 201)
Power <- bp(pi.A, p[2], n1 = n[1], n2 = n[2],
  alt = 'greater')
plot(pi.A, Power, xlab = expression(pi[A]), type  = 'l')
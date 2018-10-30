rm(list = ls())
country <- c('EU', 'Israel', 'Iran', 'North Korea', 
   'United States', 'Iraq', 'Afghanistan', 'Pakistan',
   'Syria', 'Libya', 'Saudi Arabia', 'China', 'India',
   'Russia', 'Somalia')      
yes <- c(8,59, 53, 53, 53, 52, 50, 48, 37, 36, 36, 30, 22,
   21, 16)
no <- c(89, 37, 41, 40, 44, 44, 45, 46, 56, 58, 68, 65, 74,
   76, 75 )
undecided <- c(3, 5, 5, 7, 5, 4, 6, 6, 7, 7, 7, 
   5, 5, 4, 10)
eu <- data.frame(country, yes, no, undecided)
save(eu, file = 'eu.rda')

load('eu.rda')
eu

alpha <- 0.05 ; n <- 7515
pi.0 <- 0.59 ; pi.A <- seq(0.55, 0.64, length = 201)
p.L <- qnorm(alpha / 2, pi.0, 
  sqrt(pi.0 * (1 - pi.0) / n))
power.L <- pnorm(p.L, pi.A, 
  sqrt(pi.A * (1 - pi.A) / n))
p.H <- qnorm(1 - alpha / 2, pi.0, 
  sqrt(pi.0 * (1 - pi.0) / n))
power.H <- 1 - pnorm(p.H, pi.A, 
  sqrt(pi.A * (1 - pi.A) / n))
power <- power.L + power.H

# openg()
plot(pi.A, power,type = 'l', 
   xlab = expression(italic(pi[A])), 
   ylab = 'power')
polygon(c(0.58, 0.58, 0.60, 0.60, 0.58),
   c(-1,1.1,1.1,-1,-1), col = 'gray90')
lines(pi.A, power)
abline(h = 0.01) ; abline(h = 1.04)
# saveg('binomial-power-large-sample-EU')

#test agains binom.power:
library(binom)
y <- binom.power(pi.A, n = n, p = pi.0, alpha = alpha,
  alternative='two.sided', method = 'asymp')
lines(pi.A, y, lwd = 2)

# test against bpower:
#library(Hmisc)
#lines(pi.A, bpower(pi.0, pi.A, n1 = n, n2 = n), lwd = 2)
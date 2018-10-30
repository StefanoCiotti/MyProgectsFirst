rm(list = ls())

set.seed(1) ; n <- 35 ; X <- runif(n, 0, 10) 
R <- 100000 ; p <- c(0.025, 0.05, 0.95, 0.975)

m <- matrix(sample(X, size = n * R, replace = TRUE),
  ncol = R, nrow = n)
X.bar <- apply(m, 2, mean) ; SE <- apply(m, 2, sd) / sqrt(n)
xp <- cbind(matrix(p, nrow = R, ncol = length(p),
  byrow = TRUE),   X.bar, SE)

x <- qnorm(xp[, 1 : 4], xp[, 5], xp[, 6])

openg(4.5, 4.5)
x.labels <- c(
  expression(italic(paste('lower boundary, ', alpha == 0.025))), 
  expression(italic(paste('lower boundary, ', alpha == 0.05))), 
  expression(italic(paste('upper boundary, ', alpha == 1 - 0.05))), 
  expression(italic(paste('upper boundary, ', alpha == 1 - 0.025)))
 )
y.labels <- c('density', '', 'density', '')
par(mfrow = c(2, 2))
critical.x <- matrix(ncol = 4, nrow = 4)
x.limits <- c(2,8)
xx <- seq(2, 8, length = 501)
for(j in 1 : 4){ 
  h(x[, j], xlab = x.labels[j], ylab = y.labels[j], 
    xlim = x.limits)
  lines(xx, dnorm(xx, mean(x[, j]), sd(x[, j])))
  if (j == 1) lines(xx, dnorm(xx, mean(x[, 4]), sd(x[, 4])))
  if (j == 2) lines(xx, dnorm(xx, mean(x[, 3]), sd(x[, 3])))
  if (j == 3) lines(xx, dnorm(xx, mean(x[, 2]), sd(x[, 2])))
  if (j == 4) lines(xx, dnorm(xx, mean(x[, 1]), sd(x[, 1])))
  q <- quantile(x[, j], prob = p)
  critical.x[j, ] <- q
  for(i in 1 : 4) abline(v = q[i])
}
dimnames(critical.x) <- list(c('x.L, alpha = 0.025  ',
  'x.L, alpha = 0.050  ', 'x.H, alpha = 0.950  ',
  'x.H, alpha = 0.975  '), c('0.025', '0.050', 
  '0.950', '0.975'))
print(round(critical.x, 3))
#saveg('boot-x-L-x-H', 4.5, 4.5)













#load('co2.99.rda')
#openg(4.5, 2.5)
#par(mfrow = c(1, 2))
#plot(co2.99$co2 , ylab = expression(paste(CO[2] , 
#	' metric tons per capita')))
#idx <- identify(co2.99$co2 , labels = co2.99$country)
#points(idx , co2.99$co2[idx] , pch = 19)
#h(co2.99$co2, xlab = expression(CO[2]))
#saveg('boot-CO2', 4.5, 2.5)
#
#median.fun <- function(d , i){
#	median(d[i])
#}
#
#
#d <- co2.99$co2[!is.na(co2.99$co2)]
#library(boot)
#d.boot <- boot(d , median.fun , R=10000, stype = 'i')
#(ci <- boot.ci(d.boot , conf = 0.9, type = 'perc'))
#
#
#openg()
#h(d.boot$t, xlab = expression(CO[2]))
#abline(v = ci$percent[4]) ; abline(v = ci$percent[5])
#saveg('boot-CO2-sampling-density')
#detach('package:boot')
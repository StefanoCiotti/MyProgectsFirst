rm(list = ls())
load('bmr.rda')

openg(4.5, 4.5)
par(mfrow = c(2, 2))
small <- bmr[bmr$M >= 10 & bmr$M <= 20, ]
smaller <- small[as.numeric(small[, 1]) == 8 |
  as.numeric(small[, 1]) == 16, ]
h(smaller$M, xlab = 'mass')
h(smaller$BMR, xlab = 'BMR')
plot(BMR / M ~ M, 
  data = smaller[as.numeric(smaller[, 1]) == 8, ],
  xlab = 'mass', ylab = 'BMR / mass', pch = 19, 
  col = 'red', cex = 1.5)
points(BMR / M ~ M, 
  data = smaller[as.numeric(smaller[, 1]) == 16, ],
  pch = 21, col = 'blue', cex = 1.5)

order.names <- c('Insectivora', 'Rodentia')
boxplot(BMR / M ~ as.numeric(Order), names = order.names, 
  data = smaller)
saveg('boot-EDA-BMR', 4.5, 4.5)

#openg(4.5, 2.5)
par(mfrow = c(1, 2))
library(lattice)
xyplot(BMR ~ M | Order, data = smaller,
  xlab = 'BMR', ylab = 'mass', cex = 2, pch = 19)
  
# the sampling density of BMR / M for Roidentia and insectivora
Insectivora <-  smaller[as.numeric(smaller[, 1]) == 8
  & !is.na(smaller$M), ] # 8
Insectivora <- Insectivora$BMR / Insectivora$M
Rodentia <-  smaller[as.numeric(smaller[, 1]) == 16
  & !is.na(smaller$M), ] # 16
Rodentia <- Rodentia$BMR / Rodentia$M
  
B <- 10000 ; # mean.BMR.M <- matrix(ncol = 2, nrow = B)
n <- c(length(Insectivora), length(Rodentia))

set.seed(3)
m.i <- matrix(sample(Insectivora, n[1] * B, replace = TRUE),
  ncol = B, nrow = n[1])
m.r <- matrix(sample(Rodentia, n[2] * B, replace = TRUE),
  ncol = B, nrow = n[2])
mean.BMR.M <- cbind(apply(m.i, 2, mean), apply(m.r, 2, mean))

#for(i in 1 : B){
#  mean.BMR.M[i, 1] <-
#    mean(sample(Insectivora, n[1], replace = TRUE))
#  mean.BMR.M[i, 2] <-
#    mean(sample(Rodentia, n[2], replace = TRUE))
#}

x.limits <- c(1.5, 3.5)
h(mean.BMR.M[, 1], xlab = 'Insectivora',
  xlim = x.limits, ylim = c(0, 1.25))
ci <- matrix(ncol = 2, nrow = 2)
ci[1, ] <- quantile(mean.BMR.M[, 1], prob = c(0.025, 0.975))
abline(v = ci[1, 1]) ; abline(v = ci[1, 2])
abline(v = mean(mean.BMR.M[, 1]), col = 'red', lwd = 2)
abline(v = mean(mean.BMR.M[, 2]), col = 'red', lwd = 2, lty = 2)
h(mean.BMR.M[, 2], xlab = 'Rodentia', ylab = '',
  xlim = x.limits, ylim = c(0, 3))
ci[2, ] <- quantile(mean.BMR.M[, 2], prob = c(0.025, 0.975))
abline(v = ci[2, 1]) ; abline(v = ci[2, 2])
abline(v = mean(mean.BMR.M[, 2]), col = 'red', lwd = 2)
abline(v = mean(mean.BMR.M[, 1]), col = 'red', lwd = 2, lty = 2)
#saveg('boot-CI-BMR', 4.5, 2.5)

dimnames(ci) <- list(c('Insectivora', 
  'Rodentia'),c('2.5%', '07.5%'))
ci <- round(cbind(mean = c(mean(mean.BMR.M[, 1]), mean(mean.BMR.M[, 1])),
  n = c(length(Insectivora), length(Rodentia)), ci), 2)
print(ci)



  
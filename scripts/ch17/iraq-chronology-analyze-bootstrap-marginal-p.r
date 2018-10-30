rm(list = ls())
load('simulation.rda')
load('PAi.given.Aj.rda')

s <- length(simulation)
n.locations <- length(simulation[[1]][, 1]) - 1

# marginal p
last.cols <- matrix(simulation[[1]][, n.locations + 1])
for(i in 2 : s) last.cols <- cbind(last.cols, 
  simulation[[i]][, n.locations + 1])
  
x <- seq(0, 5, length = 501)
xlim <- rbind(c(0.75, 2), c(0, 1.1), c(0, 1.5), c(3, 5), 
  c(0.25, 1.8), c(0.25, 1.45), c(0.5, 1.9), c(0.25, 1.8),
  c(1, 2.25), c(0, 1.25), c(0.25, 1.5))
  
openg(7.5, 4.5)
par(mfrow = c(3, 4))
for(i in 1 : n.locations ){
  if (i == 1 | i == 5 | i == 9) ylab = 'density' else ylab = ''
  h(last.cols[i, ], xlab = row.names(last.cols)[i],
    ylab = ylab, xlim = xlim[i, ])
  lines(x, dnorm(x, mean(last.cols[i, ]), sd(last.cols[i, ])))  
  abline(v = PAi.given.Aj[i, n.locations + 1], lwd = 2)
}
source('Iraq-chronology-empirical.R')
means <- vector()
for(i in 1 : n.locations)
  means[i] <- mean(last.cols[i, ])
lines(sort(means, decreasing = TRUE))
saveg('../docs/Iraq-chronology-analyze-bootstrap', 7.5, 4.5)
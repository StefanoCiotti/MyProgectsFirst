rm(list = ls())
load('simulation.rda')
load('PAi.given.Aj.rda')

s <- length(simulation)
n.locations <- length(simulation[[1]][, 1]) - 1

quantile.025 <- matrix(ncol = n.locations, nrow = n.locations)
quantile.975 <- quantile.025
for(location in 1 : n.locations){
  for(j in 1 : n.locations){
    v <- vector()
    for(i in 1:s){
      v[i] <- simulation[[i]][location, j]
    }
    quantile.025[location, j] <- quantile(v, probs = 0.025)
    quantile.975[location, j] <- quantile(v, probs = 0.975)
    quantile.025[location, location] <- NA  
    quantile.975[location, location] <- NA  
  }
}
rcn <- rownames(PAi.given.Aj) ; rcn <- rcn[1 : n.locations]
rownames(quantile.025) <- rcn
rownames(quantile.975) <- rcn
colnames(quantile.025) <- rcn
colnames(quantile.975) <- rcn

openg(4.5, 7.5)
par(mfrow = c(6, 2), mar = c(0,4,0,0) + 0.2)
for(i in 1 : n.locations){
  ylim <- c(0, max(quantile.975[i, ], na.rm = TRUE))
  plot(quantile.975[i, ], ylim = ylim, type = 'l',
    ylab = rcn[i], xlab = '', xaxt = 'n')
  lines(quantile.025[i, ])
  points(PAi.given.Aj[i, ])
  if (i == 2) points(c(1, 1), 
    c(quantile.975[i, 1], quantile.025[i, 1]), pch = '_', cex = 2) 
  if (i == 10) points(c(11, 11), 
    c(quantile.975[i, 11], quantile.025[i, 11]), pch = '_', cex = 2) 
  abline(v = 1:11, lty = 3)
}
saveg('../docs/iraq-chronology-analyze-bootstrap', 4.5, 7.5)

rm(list=ls())
load(file = 'temperature.rda')
source('confidence-interval.R')
X <- temperature$Lat ; Y <- temperature$JanTemp
model <- lm(Y ~ X) ; p <- 2 ; n <- length(X)
coast <- c(5, 6, 12, 13, 41, 52, 53)
openg(5, 2.75)
par(mfrow = c(1, 2))
DFBETAS <- dfbetas(model)
DFBETAS.0 <- DFBETAS[,1] ; DFBETAS.1 <- DFBETAS[, 2]
plot(model$fitted.values, DFBETAS.0,
   xlab = 'fitted', ylab = 'intercept DFBETA')
abline(h = 0) ; abline(h = 1, lty = 2)
abline(h = -1, lty = 2)
points(model$fitted.values[coast], DFBETAS.0[coast],
  pch = 19)
identify(model$fitted.values, DFBETAS.0)
plot(model$fitted.values, DFBETAS.1,
   xlab = 'fitted', ylab = 'slope DFBETA')
abline(h = 0) ; abline(h = 1, lty = 2)
abline(h = -1, lty = 2)
points(model$fitted.values[coast], DFBETAS.1[coast],
  pch = 19)
identify(model$fitted.values, DFBETAS.1)
# saveg('temperature-DFBETAS', 5, 2.75)

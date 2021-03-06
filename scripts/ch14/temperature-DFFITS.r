rm(list=ls())
load(file = 'temperature.rda')
source('confidence-interval.R')
X <- temperature$Lat ; Y <- temperature$JanTemp
model <- lm(Y ~ X) ; p <- 2 ; n <- length(X)
coast <- c(5, 6, 12, 13, 41, 52, 53)
openg()
DFFITS <- dffits(model)
plot(model$fitted.values, DFFITS,
   xlab = 'fitted', ylab = 'DFFITS')
abline(h = 0)
abline(h = 3 * sqrt(p / (n - p)), lty = 2)
abline(h = -3 * sqrt(p / (n - p)), lty = 2)
points(model$fitted.values[coast], DFFITS[coast], pch = 19)
identify(model$fitted.values, DFFITS)
#saveg('temperature-DFFITS')

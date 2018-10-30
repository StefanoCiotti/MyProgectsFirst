rm(list=ls())
load(file = 'temperature.rda')
source('confidence-interval.R')
X <- temperature$Lat ; Y <- temperature$JanTemp
model <- lm(Y ~ X) ; p <- 2 ; n <- length(X)
coast <- c(5, 6, 12, 13, 41, 52, 53)
openg()
COOK <- cooks.distance(model)
plot(COOK, xlab = 'observation number',
  ylab ="Cook's distance", type = 'h', ylim = c(0, 1))
cutoff <- qf(0.5, p, n - p, lower.tail = FALSE)
abline(h = cutoff, lty = 2)
points(coast, COOK[coast], pch = 19) ; identify(COOK)
#saveg('temperature-Cooks-distance')

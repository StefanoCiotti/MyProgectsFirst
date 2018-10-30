rm(list = ls())
load('who.fertility.mortality.rda')
d <- who.fertility.mortality
library(lattice)
windows()
xyplot(d[, 9] ~ d[, 7] | d[, 3], xlab = '% growth',
  ylab = 'death rate', par.strip.text = list(cex = 0.6))
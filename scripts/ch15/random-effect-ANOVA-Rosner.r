rm(list = ls())
source('random-effect-ANOVA.R')

# Rosner p. 556
group <- rep(1 : 5, 2)
replication <- c(rep(1, 5), rep(2, 5))
response <- log(c(25.5, 11.1, 8, 20.7, 5.8, 30.4, 15, 8.1,
  16.9, 8.4))
  
library(lattice)
trellis.device(color = FALSE, width = 4, height = 4)
xyplot(response ~ replication | group, type = 'b', cex = 0.8)
saveg('random-effect-ANOVA-Rosner', 4, 4)

cbind(group, replication, response)
re.1w(group, replication, response)



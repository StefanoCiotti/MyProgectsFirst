rm(list = ls())
e <- read.csv('elections-2000.csv')
head(e)
stacked.e <- cbind(stack(e), county = e$County)
head(stacked.e)
plot(stacked.e[, 2 : 1])


rm(list=ls())
dbh <- read.table('DBH.txt', header = FALSE, sep = '\t')
names(dbh) <- c('DBH', 'height', 'age')
attach(dbh)
library(RcmdrPlugin.HH)

openg(4.75)
X <- log(age) ; Y <- log(height)
set.seed(1)
idx <- sample(1 : length(X), 5)
X <- X[idx] ; Y <- Y[idx]
model <- lm(Y ~ X)
ci.plot(model, main = '')
saveg('trees-height-vs-age-with-CI', 4.5)

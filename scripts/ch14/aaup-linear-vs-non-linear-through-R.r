rm(list = ls())
load('aaup.rda')
source('confidence-interval.R')
source('see.R')

X <- aaup[, 5] ; Y <- aaup[, 13]
test <- (is.na(X) == FALSE & is.na(Y) == FALSE)
X <- X[test] ; Y <- Y[test]

see(X, Y,'AAUP-PROFS-VS-SALARY')
log.X <- log(X) ; log.Y <- log(Y) ; model<-lm(log.Y ~ log.X)
see(log.X, log.Y, 'AAUP-PROFS-VS-SALARY-LOG')

openg()
qqnorm(residuals(model))
qqline(residuals(model))
saveg('AAUP-PROF-VS-SALARY-QQ-RESIDUALS')

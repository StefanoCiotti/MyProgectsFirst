rm(list=ls())
load(file = 'temperature.rda')
source('confidence-interval.R')

X <- temperature$Lat
Y <- temperature$JanTemp
n <- length(X)
summary(model <- lm(Y ~ X))

# line and standardized residuals
coast <- c(5, 6, 12, 13, 41, 52, 53)
openg(5, 2.75)
par(mfrow = c(1, 2))
ci.lm(X, model)
points(X[coast], Y[coast], pch = 19)
identify(X, Y)
residuals.standardized <- rstandard(model)
plot(model$fitted.values, residuals.standardized,
   xlab = 'fitted', ylab = 'residuals')
abline(h=0)
points(model$fitted.values[coast],
  residuals.standardized[coast], pch = 19)
text(model$fitted.values[coast],
  residuals.standardized[coast], labels = coast,pos = 4)
identify(model$fitted.values,
  residuals.standardized)
#saveg('temperature-RSTANDARD',5,2.75)

# QQ on standardized residuals
openg()
q <- qqnorm(residuals(model))
identify(q$x, q$y)
qqline(residuals(model))
#saveg('temperature-RSTANDARD-QQ-RESIDUALS')



# standardized vs RSTUDENT
openg()
RSTUDENT<-rstudent(model)
plot(model$fitted.values, RSTUDENT,
   xlab = 'fitted', ylab = 'standard residuals')
abline(h = 0) ; abline(h = 2, lty = 2)
points(model$fitted.values[coast], RSTUDENT[coast],
  pch = 19)
points(model$fitted.values, residuals.standardized, cex = 2)
identify(model$fitted.values, RSTUDENT)
#saveg('temperature-RSTANDARD-vs-RSTUDENT')

rm(list = ls())
load('trisomy.rda')

X <- trisomy$age
Y <- as.factor(trisomy$trisome)
openg()
plot(Y, X, xlab = 'trisomy', ylab = 'standardized age')
# saveg('trisomy-eda')

library(Design)
ddist <- datadist(X, Y)
options(datadist = 'ddist')
model <- lrm(Y ~ X, x = TRUE, y = TRUE, se.fit = TRUE)
print(model)
openg()
plot(model, xlab = 'standardized age', 
	ylab = 'log odds of trisomy')
# saveg('trisomy-lrm')
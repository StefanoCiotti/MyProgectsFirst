rm(list = ls())
load('trisomy.rda')
X <- trisomy$age
Y <- as.factor(trisomy$trisome)
set.seed(22) ; idx <- sample(1 : length(X), 5)
cbind(X = X[idx], Y = Y[idx])

library(Design)
ddist <- datadist(X, Y)
options(datadist = 'ddist')
model <- lrm(Y ~ X, x = TRUE, y = TRUE, se.fit = TRUE)

openg()
plot(model$x, 1 / (1 + exp(-model$linear.predictors)),
   type = 'l', ylim = c(0, 1),
   xlab = 'maternal standardized age', 
   ylab = 'probability of trisomy')
se <- 1.96 * model$se.fit
lines(model$x, 1 / (1 + exp(-(model$linear.predictors +
   se ))), lty = 2)
lines(model$x, 1 / (1 + exp(-(model$linear.predictors - 
   se))), lty = 2)

tri <- split(X, Y)
head(tri$'TRUE')
n.true <- tapply(tri$'TRUE', as.factor(tri$'TRUE'),
  length)
n.false <- tapply(tri$'FALSE', as.factor(tri$'FALSE'),
  length)
n <- n.true + n.false
true <- n.true / n
false <- n.false / n

points(unique(model$x), true, pch = 19)
points(unique(model$x), false)
# saveg('trisomy-logistic-regression')






















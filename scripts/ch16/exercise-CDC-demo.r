rm(list = ls()) ; options(width = 60)
load('demo_d.short.rda')
idx <- complete.cases(demo_d.short[, c(1, 10, 11)])
d <- demo_d.short[idx, c(1, 10, 11)]


X <- (d[, 2] + d[, 3]) / 2 / 1000
X[X == Inf] <- 75
Y <- d[, 1] ; Y[Y == 'Male'] <- 0 ; Y[Y == 'Female'] <- 1
Y <- as.numeric(Y)



library(Design)
ddist <- datadist(X, Y)
options(datadist = 'ddist')
(model <- lrm(Y ~ X, x = TRUE, y = TRUE, se.fit = TRUE))

openg()
plot(model)
saveg('exercise-CDC-demo-model')

plot(model$x, 1 / (1 + exp(-model$linear.predictors)),
  type = 'l', ylim = c(0.48, 0.54),
  xlab = 'mean household income',
  ylab = 'probability of female')
se <- 1.96 * model$se.fit
points(model$x, 1 / (1 + exp(-(model$linear.predictors +
  se ))), lty = 2)
points(model$x, 1 / (1 + exp(-(model$linear.predictors -
  se))), lty = 2)
tri <- split(X, Y)
head(tri$'1')
n.true <- tapply(tri$'1', as.factor(tri$'1'),
  length)
n.false <- tapply(tri$'0', as.factor(tri$'0'),
  length)
n <- n.true + n.false
true <- n.true / n
false <- n.false / n
points(unique(model$x), true, pch = 19)
#points(unique(model$x), false, pch = 19)



p <- 1 / (1 + exp(-model$linear.predictors))
hat <- residuals.lrm(model, type = 'hat')
pearson <- residuals.lrm(model, type = 'pearson')
deviance <- residuals.lrm(model, type = 'deviance')


standard.pearson <- pearson / (sqrt(1 - hat))
delta.C <- standard.pearson^2
delta.D <- deviance^2 / (1 - hat)
delta.beta <- residuals.lrm(model, type = 'dfbeta')
delta.beta.0 <- delta.beta[, 1]
delta.beta.1 <- delta.beta[, 2]

delta.C.lim <- c(min(delta.C), max(delta.C))
delta.D.lim <- c(min(delta.D), max(delta.D))
delta.beta.0.lim <- c(min(delta.beta.0), max(delta.beta.0))
delta.beta.1.lim <- c(min(delta.beta.1), max(delta.beta.1))



#openg(4.5, 4.5)
par(mfrow=c(2, 2))
plot(p[y == 1], delta.C[y == 1],
  xlab = expression(italic(hat('P'))),
  ylab = expression(paste(Delta,italic('C'))),
  ylim = delta.C.lim, cex = 2)
points(p[y == 0], delta.C[y == 0])
plot(p[y == 1], delta.D[y == 1],
  xlab = expression(italic(hat('P'))),
  ylab = expression(paste(Delta,italic('D'))),
  ylim = delta.D.lim, cex = 2)
points(p[y == 0], delta.D[y == 0])
plot(p[y == 1], delta.beta.0[y == 1],
  xlab = expression(italic(hat('P'))),
  ylab = expression(paste(Delta,italic(beta[0]))),
  ylim = delta.beta.0.lim, cex = 2)
points(p[y == 0], delta.beta.0[y == 0])
plot(p[y == 1], delta.beta.1[y == 1],
  xlab = expression(italic(hat('P'))),
  ylab = expression(paste(Delta,italic(beta[1]))),
  ylim = delta.beta.1.lim, cex = 2)
points(p[y == 0], delta.beta.1[y == 0])
#saveg('fish-residuals-SFS', 4.5, 4.5)




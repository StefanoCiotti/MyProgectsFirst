rm(list = ls())
load('fish.rda')
x <- fish$adults$depth
y <- ifelse(fish$adults$SFS>0, 1, 0)

library(Design)
ddist <- datadist(x, y)
options(datadist = 'ddist')
model <- lrm(y ~ x, x = TRUE, y = TRUE, se.fit = TRUE)

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




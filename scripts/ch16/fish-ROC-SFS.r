load('fish.rda')

# random sample of 10 observations
idx<-sample(dimnames(fish$adults)[[1]], 10)
col<-c(1, 4, 6, 8, 9, 66)
fish$adults[idx, col]

X <- fish$adults$depth
Y <- ifelse(fish$adults$SFS > 0, 1, 0)

# eda
openg(4, 4)
par(mfrow = c(2, 2))
BW <- 5 ; ylim <- c(0, 0.02) ; xlim <- c(0, 200)
hist(X, main = 'available', xlab = 'depth (cm)',
   ylab = 'density', freq = FALSE,
   xlim = xlim, ylim = ylim)
lines(density(X, bw = BW), lwd = 3)
hist(X[Y == TRUE], main = 'SFS present',
  xlab = 'depth (cm)', ylab = 'density', freq = FALSE,
  xlim = xlim, ylim = ylim)
lines(density(X, bw = BW), lwd = 3)
lines(density(X[Y == TRUE]))
hist(X[Y == FALSE], main = 'SFS absent',
  xlab = 'depth (cm)',    ylab = '', freq = FALSE,
  xlim = xlim, ylim = ylim)
lines(density(X, bw = BW), lwd = 3)
lines(density(X[Y == FALSE]))

# fit lrm
library(Design)
ddist <- datadist(X, Y)
options(datadist = 'ddist')
model <- lrm(Y ~ X, x = TRUE, y = TRUE, se.fit = TRUE)
plot(model, xlab = 'water depth (cm)', 
	ylab = 'log odds of SFS present')
#saveg('fish-SFS-eda', 4, 4)

# ROC
p.hat <- 1 / (1 + exp(-model$linear.predictors))

# this way
BY <- (max(p.hat) - min(p.hat)) / 200
p.cuts <- seq(min(p.hat), max(p.hat), by = BY)[-1]



sen <- spe <- vector(length = length(p.cuts))
for(i in 1 : length(p.cuts)){
   tb <- table(Y, p.hat >= p.cuts[i])
   if(i <= 4) print(tb)
   sen[i] <- tb[2,2] / (tb[2, 1] + tb[2, 2])
   spe[i] <- tb[1, 1] / (tb[1, 1] + tb[1, 2])
}

openg(4.5)
par(mfrow=c(1, 2))
plot(p.cuts, sen, type = 'l', xlab='cut-off probabilities',
   ylab = 'sensitivity or specificity')
lines(p.cuts, spe)
abline(v = p.cuts[(spe > sen) & (sen > (spe - 0.01))])
text(locator(), label = c('sensitivity', 'specificity'),
   pos=c(2, 2))
   
library(verification)
roc.plot(Y, p.hat, main = '', xlab = 'specificity = 1',
  ylab = 'sensitivity', cex = 2)
(area<- roc.area(Y, p.hat))
#saveg('fish-ROC-SFS', 4.5)

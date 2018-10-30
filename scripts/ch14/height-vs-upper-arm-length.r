rm(list=ls())
rm(list = ls())
load('bm.rda')
load('bmv.rda')

trimmed <- apply(as.array(bmv[, 2]), 1, function(x)
  if (substr(x,   nchar(x), nchar(x)) == ' ')
  strtrim(x, nchar(x) - 1) else strtrim(x, nchar(x)))
bmv[, 2] <- unlist(trimmed)

trimmed <- apply(as.array(as.character(bmv[, 1])), 1,
  function(x)
  if (substr(x,   nchar(x), nchar(x)) == ' ')
  strtrim(x, nchar(x) - 1) else strtrim(x, nchar(x)))
bmv[, 1] <- unlist(trimmed)
head(bmv, 4)

Y <- bm[, 16] ; X <- bm[, 9]
openg(4.5, 2.75)
par(mfrow=c(1, 2))
plot(X, Y)
log.X <- log(X) ; log.Y <- log(Y)
plot(log.X,log.Y)
model <- lm(log.Y ~ log.X)
abline(reg = model)
idx <- identify(log.X, log.Y, n = 4)
#saveg('upper-arm-vs-height-full-data',4.5,2.75)
print(summary(model))

idx <- c(2174, 3499, 4779, 6309)
openg(5, 5)
log.X <- log.X[idx] ; log.Y <- log.Y[idx]
plot(log.X, log.Y, xlim = c(4.6, 5.05),
  xlab='log(height)', ylab='log(upper arm length)')
model <- lm(log.Y ~ log.X)
abline(reg = model)
for(i in 1:length(log.X))
   lines(c(log.X[i], log.X[i]),
   c(log.Y[i], model$fitted.values[i]))
points(log.X, model$fitted.values, pch = 19)
text(log.X, model$fitted.values,
  labels = round(model$fitted.values, 3), pos = 4)
text(log.X, log.Y, labels=round(log.Y, 3),
  pos = 4)
for(i in 1 : length(log.X)){
   if(i == 2 | i == 4) pos = 4 else pos = 2
   text(log.X[i], 
    (model$fitted.values[i] + log.Y[i]) / 2,
      labels=bquote(epsilon[.(i)]==.(round(log.Y[i] -
      model$fitted.values[i], 3))), pos = pos)
}
#saveg('upper-arm-vs-height-4-points',5,5)





d<-data.frame(log.X, log.Y, predict(model),
  log.Y - predict(model), (log.Y - predict(model))^2)
names(d)<-c('~~~~$X$~~~~','~~~~$Y$~~~~','~~~~$E[Y|X]$~~~~','~~~~$\\varepsilon$~~~~','~~~~$\\varepsilon^2$~~~~')
print(sum((log.Y-predict(model))^2))

library(Hmisc)

latex(d,
   file='upper-arm-length-vs-height.tex',
   label='table: error SSQ',
   cdec=rep(3,5), 
   col.just=rep('r',5),
#   rowname=c('Death (all cause)','Cancer Death',
#      'Relapse','Hospitalization'),
#   rgroup=c('Fatal Events','Non--fatal Events'),
   rowlabel='$i$', 
   caption='log upper arm length ($Y$) vs log height ($X$) and the expected values of Y according to (\\protect\\ref{eq: E[Y] for upper arm vs height}).',
   na.blank=TRUE,
   landscape=FALSE,
   where="!htbp",
   ctable=TRUE)

v<-var(d[,1:2])
b1<-v[1,2]/v[1,1]
b0<-mean(d[,2])-b1*mean(d[,1])

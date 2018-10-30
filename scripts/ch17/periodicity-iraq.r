rm(list = ls())
load('iraq.rda')
d <- iraq

fo <- function(x, m, a, start, periods){
   m + a * sin((x - start)/periods)
}

library(evd)
n <- length(iraq[, 1])
x <- 1 : n
m <- mean(OT); a<-  - 30; b <- 70; c<-  - 3; d <- 28

OT.nls <- nls(OT ~ fo(x, m, a, b, c, d), 
   start = list(a = a, b = b, c = c, d = d))
OT.co <- c(mean = m, coefficients(OT.nls))
summary(OT.nls)

m <- mean(YM); a<-  - 30; b <- 70; c<-  - 3; d <- 28
YM.nls <- nls(YM ~ fo(x, m, a, b, c, d), 
   start = list(a = a, b = b, c = c, d = d))
YM.co <- c(mean = m, coefficients(YM.nls))
summary(YM.nls)

OT.r <- OT - fo(x, OT.co[1], OT.co[2], OT.co[3], OT.co[4], 
   OT.co[5])
YM.r <- YM - fo(x, YM.co[1], YM.co[2], YM.co[3], YM.co[4], 
   YM.co[5])

w = 5; h = 5
openg(w, h)
par(mfrow = c(2, 2))
plot(OT[1 : (3 * 365)], pch = '.', xlab = '', 
   ylab = 'max. air - temp.', main = 'Otter Tail')
x <- 1 : (3 * 365)
lines(x, fo(x, OT.co[1], OT.co[2], OT.co[3], OT.co[4], 
   OT.co[5]))
plot(YM[1 : (3 * 365)], pch = '.', xlab = '', ylab = '', 
   main = 'Yellow Medicine')
x <- 1 : (3 * 365)
lines(x, fo(x, YM.co[1], YM.co[2], YM.co[3], YM.co[4], 
   YM.co[5]))
plot(x, OT.r[1 : (3 * 365)], 
   xlab = 'day', ylab = 'residuals', pch = '.')
abline(h = 0)
plot(x, YM.r[1 : (3 * 365)], 
   xlab = 'day', ylab = '', pch = '.')
abline(h = 0)
saveg(
	'../docs/AnalysisOfExtremeValues/EV-temp-max-detrended', 
	w, h)
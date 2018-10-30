rm(list = ls())
load('SO2.rda')

openg()
plot(SO2[, 2], SO2[, 3], axes = FALSE,
  xlab = expression(SO[2]),
  ylab = 'city', ylim = c(1, 3.2),
  xlim = c(0, 160), cex = 1.2)
axis(1)
axis(2, at = c(1, 2, 3),
  labels = c('Berlin', 'Madrid', 'Roma'))
abline(h = c(1, 2, 3))
means <- tapply(SO2[, 2], SO2[, 3], mean)
points(means, 1 : 3, pch = '|', cex = 2)
Y.bar.bar <- mean(SO2[, 2]) ;
lines(c(Y.bar.bar, Y.bar.bar), c(0, 3.01))
labels <- c(expression(bolditalic(bar(Y)[1])),
  expression(bolditalic(bar(Y)[2])),
  expression(bolditalic(bar(Y)[3])),
  expression(bolditalic(bar(bar(Y)))))
text(c(means, Y.bar.bar), c(1 : 3, 3), labels = labels,
  pos = 3)
text(SO2[26, 2], 2,
  labels = expression(bolditalic(Y[2*j])),
  pos = 3)
saveg('EU-SO2-Berlin-Madrid-Roma')
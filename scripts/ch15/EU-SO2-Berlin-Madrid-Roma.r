rm(list = ls())
load('EU.rda')
load('EU.station.rda')

m <- match(toupper(EU.station[, 10]),
  c('MADRID', 'ROMA', 'BERLIN'), nomatch = 0)
stations <- EU.station[m > 0, 1]

tmp <- EU[EU$statistic_shortname == 'Max' &
  EU$component_caption == 'SO2', ]
m <- match(tmp[, 1], stations, nomatch = 0)
tmp <- tmp[m > 0, c(1, 8, 15)]
tmp <- tmp[tmp[, 2] == 2005, c(1, 3)] ; head(tmp)

n  <- tapply(substr(tmp[, 1], 1, 2), substr(tmp[, 1], 1, 2),
  length)
city <- c(rep('Berlin', n[1]), rep('Madrid', n[2]),
  rep('Roma', n[3]))
SO2 <- cbind(tmp, city)
names(SO2)[1:2] <- c('station', 'Max SO2') ; head(SO2, 3)
save(SO2, file = 'SO2.rda')

openg()
plot(SO2[, 2], SO2[, 3], axes = FALSE, xlab = 'SO2',
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
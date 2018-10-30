rm(list = ls())
load('SO2.rda')
names(SO2)

(n <- tapply(SO2$'Max SO2', SO2$city, length))

set.seed(101) ; n.c <- cumsum(n)
i.1 <- sample(1 : n.c[1], n[3])
i.2 <- sample((n.c[1] + 1) : n.c[2], n[3])
i.3 <- (n.c[2] + 1) : n.c[3]
d <- SO2[c(i.1, i.2, i.3), ]
(vars <- tapply(d[, 2], d[, 3], var))

(vars.ratio <- c(vars[1]/vars[2],
  vars[1] / vars[3], vars[3] / vars[2]))
(p.L <- c(pf(vars.ratio[1], 5, 5),
  pf(vars.ratio[2], 5, 5),
  pf(vars.ratio[3], 5, 5)))

openg(4.5, 2.5)
par(mfrow = c(1, 3))
qqnorm(d[d[, 3] == 'Berlin', 2], main = 'Berlin')
qqline(d[d[, 3] == 'Berlin', 2])
qqnorm(d[d[, 3] == 'Madrid', 2], main = 'Madrid', ylab = '')
qqline(d[d[, 3] == 'Madrid', 2])
qqnorm(d[d[, 3] == 'Roma', 2], main = 'Roma', ylab = '')
qqline(d[d[, 3] == 'Roma', 2])
saveg('EU-SO2-Berlin-Madrid-Roma-ANOVA', 4.5, 2.5)

a <- aov(d$'Max SO2' ~ d$city, data = d)
summary(a)

library(granova)
openg(4.5, 3.5)
granova.1w(d$'Max SO2', d$city)
saveg('EU-SO2-Berlin-Madrid-Roma-ANOVA-plot', 4.5, 3.5)


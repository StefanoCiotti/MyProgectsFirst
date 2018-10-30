rm(list = ls())
load('demo_d.short.rda')

d <- demo_d.short
names(d[, c(4, 10, 11)])
i <- d[, c(4, 10, 11)]
i <- i[complete.cases(i), ]
i[, 2] <- i[, 2] / 1000
i[, 3] <- i[, 3] / 1000 + 0.001

i <- data.frame(i[, 1], i[, 2] + (i[, 3] - i[, 2])/2)
names(i) <- c('ethnicity', 'income')
i <- i[i$income <= 75, ]
attach(i)

options(width = 60)
(n <- tapply(income, ethnicity, length))
w <- n[c(3, 5)]
(L.1 <- w / sum(w))
nw <- n[c(1, 2, 4)]
(L.2 <- nw / sum(nw))
round(sum(c(L.1, -L.2)), 3)
contr <- c(L.1, -L.2)
(contrasts <- contr[c(3, 4, 1, 5, 2)])

(v <- var(income))
(SE <- sqrt(v * sum(contrasts^2 / n)))
means <- tapply(income, ethnicity, mean)
cbind(group.means = means, contrast = contrasts)
L <- sum(contrasts * means)
T.L <- L / SE ; df <- sum(n) - length(n)
p.value <- 1 - pt(T.L, df)
c(T.L = T.L, df = df, p.value = p.value)

library(granova)
openg(4.5, 3.5)
granova.1w(i$'income', i$ethnicity)

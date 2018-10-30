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

a <- aov(income ~ ethnicity, data = i)
hsd <- TukeyHSD(a)
openg(4.5, 2.75)
par(mar = c(5, 23, 3, 4), cex.main = 1)
plot(hsd, las = 2)
saveg('CDC-Tukey-HSD', 4.5, 2.75)


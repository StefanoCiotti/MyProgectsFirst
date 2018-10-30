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
n <- tapply(income, ethnicity, length)
k <- length(n)
df <- sum(n) - k
means <- tapply(income, ethnicity, mean)

con <- contrasts(ethnicity, 6) <- c(1, 1, 0, 0, 0)
(a <- aov(income ~ ethnicity, contrasts = TRUE))

Within.MS <- sum(a$residuals^2) / a$df.residual
SE <- sqrt(Within.MS * (1/n[1] + 1/n[3]))
T.1.3 <- as.numeric((means[1] - means[3]) / SE)
alpha <- 0.05
adjust <- choose(5, 2)
alpha <- alpha / adjust
c(T = T.1.3, df = df, p.value = 1 - pt(abs(T.1.3), df),
  alpha = alpha)

a <- aov(income ~ ethnicity, data = i)
hsd <- TukeyHSD(a)
openg(4.5, 2.75)
par(mar = c(5, 20, 3, 2), cex.main = 1)
plot(hsd, las = 2)
saveg('CDC-Tukey-HSD', 4.5, 2.75)


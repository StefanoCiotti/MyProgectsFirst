rm(list = ls())
load('demo_d.short.rda')
library(agricolae)
#example(LSD.test)
#example(bar.group)

d <- demo_d.short
names(d[, c(4, 10, 11)])
i <- d[, c(4, 10, 11)]
i <- i[complete.cases(i), ]
i[, 2] <- i[, 2] / 1000
i[, 3] <- i[, 3] / 1000 + 0.001

i <- data.frame(i[, 1], i[, 2] + (i[, 3] - i[, 2])/2)
names(i) <- c('ethnicity', 'income')

openg(4.5, 3)
par(mfrow = c(1, 2))
barplot(table(i$income), las = 2,
  xlab = 'mean income category (in $1,000)',
  ylab = 'count')
i <- i[i$income <= 75, ]
barplot(table(i$income), las = 2,
  xlab = 'mean income category (in $1,000)')
saveg('CDC-demographics-LSD-bar-plot', 4.5, 3)

attach(i)
openg(4.5, 4.5)
par(mar = c(15, 4, 1, 2))
plot(i, las = 2, ylim = c(0, 80), xlab = '',
  ylab = 'household yearly income, from (in $1,000)')
saveg('CDC-demographics-LSD-box-plot', 4.5, 4.5)

model <- aov(income ~ ethnicity, data = i)
summary(model)
par(mfrow = c(2,2))
plot(model)
df<-df.residual(model)
MS.error<-deviance(model)/df
LSD.test(income, ethnicity, df, MS.error, group = FALSE,
  main = 'household income\nvs skin color/ethnicity')
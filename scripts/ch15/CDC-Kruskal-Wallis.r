rm(list = ls())
(income <- read.table('CDC-demographics-income-categories.txt',
  header = TRUE, sep = '\t'))
  
load('demo_d.short.rda')
d <- demo_d.short[, c(4, 9)]
idx <- which(as.numeric(d[, 2])>11) ; d[idx, 2] <- NA
d <- d[complete.cases(d), ]
head(d)

#openg(4.5, 4)
par(mfrow = c(1, 2), mar = c(15, 4, 1, 2))
barplot(table(d$'Race/Ethnicity'), las = 2, ylim = c(0, 4000))
barplot(table(d$'Annual Household Income'), las = 2,
  names.arg = income$income[as.numeric(income$code) <= 11],
  ylim = c(0, 2500))
#saveg('CDC-Kruskal-Wallis-bar-plot', 4.5, 4)

kruskal.test(as.integer(d[, 2]) ~ as.factor(d[, 1]))
library(pgirmess)
kruskalmc(d[, 2], d[, 1])
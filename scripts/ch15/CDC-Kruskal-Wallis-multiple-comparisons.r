rm(list = ls())
(income <- read.table('CDC-demographics-income-categories.txt',
  header = TRUE, sep = '\t'))
load('demo_d.short.rda')
d <- demo_d.short[, c(4, 9)]
idx <- which(as.numeric(d[, 2])>11) ; d[idx, 2] <- NA
d <- d[complete.cases(d), ]

library(pgirmess)
kruskalmc(as.integer(d[, 2]), as.factor(d[, 1]))

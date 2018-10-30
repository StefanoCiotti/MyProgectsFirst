rm(list = ls())
rm(list = ls())
load('demo_d.short.rda')

d <- demo_d.short
names(d[, c(4, 10, 11)])
no.NA <- !is.na(d[, 4]) & !is.na(d[, 10]) & !is.na(d[, 11])
i <- data.frame(as.factor(d[no.NA, 4]), d[no.NA, 10]/1000,
  d[no.NA, 11]/1000 + 0.001)
i <- data.frame(i[, 1], i[, 2] + (i[, 3] - i[, 2])/2)
names(i) <- c('ethnicity', 'income')
i <- i[i$income <= 75, ]

source('Bonferroni.R')

(b <- Bonferroni(i))
      
      

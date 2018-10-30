rm(list = ls())

options(width = 60)
who <- read.csv('who.by.continents.and.regions.txt',
  sep = '\t', header = TRUE)
head(who[, c('country','continent','region')], 5)
tbl <- table(who[, c('region','continent')])
set.seed(102)
(s.tbl <- tbl[sample(1 : length(tbl[, 1]), 2), ])

library(epitools)
expand.table(s.tbl)


load('capital.punishment.rda')
cp <- capital.punishment
(tbl <- table(cp[, c('Method', 'Sex')]))
set.seed(102)
(s.tbl <- tbl[sample(1 : length(tbl[, 1]), 2), ])
library(epitools)
expand.table(s.tbl)

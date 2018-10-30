rm(list = ls())
who <- read.table('who.by.continents.and.regions.txt',
  sep = '\t', header = TRUE)
head(who[, 1 : 3], 4)
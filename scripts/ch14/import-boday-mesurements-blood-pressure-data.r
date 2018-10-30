rm(list = ls())

bm <- read.xport('body-measurements.xpt')
bmv <- read.table('body-measurements-variables.txt',
  sep = '\t', header = TRUE, skip = 2)

save(bm, file = 'bm.rda')
save(bmv, file = 'bmv.rda')

rm(list = ls())
# library(CTFS)
load('iraq.dead.rda')
general.location <- read.table('general.location1.txt',
 header = TRUE, sep = '\t')
 iraq.dead <- iraq.dead[, 1 : 11]
iraq.dead <- data.frame(iraq.dead, 
  general.location = general.location[, 1])
save(iraq.dead, file = 'iraq.dead.rda')

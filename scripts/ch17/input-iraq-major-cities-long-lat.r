rm(list = ls())
iraq.long.lat <- read.table('major-cities-long-E-lat-N.txt', 
  header = TRUE, sep = '\t')
head(iraq.long.lat)
save(iraq.long.lat, file = 'iraq.long.lat.rda')

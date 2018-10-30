rm(list = ls())

EU.station <- read.table('Airbase_v1_station.txt',
  header = TRUE, sep = ',', stringsAsFactors = FALSE)
save(EU.station, file = 'EU.station.rda')
EU <- read.table('Airbase_v1_statistic.txt', header = TRUE,
   sep = ',', stringsAsFactors = FALSE)
save(EU, file = 'EU.rda')

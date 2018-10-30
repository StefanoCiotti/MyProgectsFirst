rm(list = ls())

set.seed(211)
(Germany <- paste('DE', round(runif(5, 0, 1000)), 'A',
  sep = ''))
(Italy <- paste('IT', round(runif(5, 0, 1000)), 'A',
  sep = ''))
stations <- c(Italy, Germany)
stations
stations[substr(stations, 1, 2) == 'DE'] <- 'Germany'
stations[substr(stations, 1, 2) == 'IT'] <- 'Italy'
stations

paste('axiom', ':', ' power', ' corrupts', sep = '')

(x <- c('for;crying;out;loud', 'consistency;is;not;a virtue'))
rbind(strsplit(x, ';')[[1]], strsplit(x, ';')[[2]])








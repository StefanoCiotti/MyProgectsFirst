rm(list = ls())
load('EU.station.rda')
library(maps)
library(mapdata)

#Berlin, Germany	52 	30 N 	13 	25 E 
#Madrid, Spain	40 	26 N 	3 	42 W
#Rome, Italy	41 	54 N 	12 	27 E 

cities.long <- c(13 + 25 / 60, -(3 + 42/60),
  12 + 27 / 60)
cities.lat <- c(52.5, 40 + 26 / 60, 41 + 54 / 60)

openg(3, 3)
par(mar = c(4,4,0,0) + 0.1)
r <- c('Spain', 'Italy', 'Germany')
col<-colors()[c(360, 360, 360)]
map('world', regions = r, fill = TRUE, col = col)
map.axes()
text(c(-2, 7, 3), c(36, 43, 51), labels = r)

m <- match(EU.station[, 3], c('ITALY', 'SPAIN', 'GERMANY'),
  nomatch = 0)
long <- EU.station[m > 0, 6]
lat <- EU.station[m > 0, 7]
points(long,lat, col=c(360))
points(cities.long, cities.lat, col = 'black',
  pch = 19, cex = 1.5)
saveg('EU-station-map', 3, 3)
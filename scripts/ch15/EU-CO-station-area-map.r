rm(list = ls())
load('EU.CO.rda')
load('EU.station.rda')
library(maps)
library(mapdata)

openg(4.5, 4)
r <- c('Belgium', 'France', 'Germany', 'Italy', 'Spain', 'UK')
col<-c('grey95', 'grey90')
par(mar = c(4,2,0,0) + 0.1)
m<-map('world', regions = r, fill = TRUE, col = col,
  xlim = c(-15, 20), ylim = c(35,62))
map.axes()
points(EU.CO$long,EU.CO$lat, col = 'grey60')
for(i in 1 : 6) map.cities(country = r[i], capitals = 1,
  cex = 1.25)
ll <- world.cities[world.cities[, 1] == 'London' & world.cities[, 2] == 'UK', 4 :5]
points(ll[2], ll[1])
text(ll[2], ll[1], labels = 'London', pos = 2, cex = 1.5)
saveg('EU-CO-station-map', 4.5, 4)
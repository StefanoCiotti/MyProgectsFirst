rm(list = ls())
load('iraq.long.lat.rda')
library(maps)
library(mapdata)
#load('points91')
#load('points99')
#load('points02')
#
# from http://www.mapsofworld.com/lat_long/iraq-lat-long.html
# iraq's coordinates are 33 o 00' N and 44 o 00' E

openg(4.5, 4.5)
par(mar = c(4,0,0,0) + 0.1)
r <- c('Iraq', 'Saudi Arabia', 'Kuweit','Iran',
   'Turkey', 'Syria', 'Jordan')
col<-colors()[seq(7,length(colors()),by=2)]
labels<-r
m<-map('world2Hires', xlim = c(38.5, 48.5), 
  ylim=c(28, 38), fill = TRUE, regions = r,
  col = col, exact = FALSE)
map.scale()
map.axes()
loc <- list(x = c(41.84208, 39.02169, 47.13982, 46.56812, 
  39.36471, 39.02169, 38.64056),
  y = c(32.37467, 30.00165, 29.26409, 34.23461, 37.40933, 
    35.00424, 32.43881))
text(loc, labels = r, pos = 4)
places <- c(66, 73, 13, 45, 68, 41, 71, 112, 130, 142)
pos <- c(4, 4, 1, 2, 4, 2, 4, 4, 2, 2)
points(iraq.long.lat$Longitude[places], 
  iraq.long.lat$Latitude[places])
text(iraq.long.lat$Longitude[places], 
  iraq.long.lat$Latitude[places], 
  labels = iraq.long.lat$Location[places], pos = pos)
saveg('../docs/Iraq-map', 4.5, 4.5)


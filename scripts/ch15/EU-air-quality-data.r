rm(list = ls())
options(width = 60)
load('EU.station.rda')
names(EU.station)
load('EU.rda')
names(EU)[c(1,2,3,4,6,9,12,13,14,15)]
head(EU[c(3, 13, 15)])
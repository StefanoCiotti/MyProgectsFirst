rm(list = ls())
openg()
load('Iraq.cnts.rda')
acf(cnts, lag.max = 50)
saveg('lag-plots-Iraq')





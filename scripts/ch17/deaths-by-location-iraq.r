rm(list = ls())
# library(CTFS)
load('iraq.dead.hostile.rda')

freq.by.location <- as.data.frame(
  table(iraq.dead.hostile$Where))
names(freq.by.location) <- c('Location', 'Deaths')

openg(4.5, 4.5)
plot(freq.by.location$Deaths, xaxt = 'n', main = '', 
  xlab = 'Location', ylab = 'Deaths')
abline(h = 0)

#locations <- identify(freq.by.location$Deaths,
#  labels = freq.by.location$Location,
#  pos = TRUE, atpen = TRUE)
 
# Deadliest locations 
idx <- c(7, 42, 60, 62, 66, 69, 89, 136, 161, 177, 195)
pos <- c(4, 2, 2, 4, 3, 4, 4, 4, 4, 4, 4)
text(idx, freq.by.location$Deaths[idx], 
  labels = freq.by.location$Location[idx], pos = pos) 
points(idx, freq.by.location$Deaths[idx], pch = 19, cex = 1.5)
saveg('../docs/deaths-by-location', 4.5, 4.5)
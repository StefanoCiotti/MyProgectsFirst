rm(list = ls())
library(CTFS)
load('iraq.dead.rda')
load('iraq.daily.deaths.rda')
julian <- iraq.daily.deaths$Julian
# Deadliest locations obtained from  
# levels(iraq.dead$general.location)
idx <- c(7, 42, 60, 62, 66, 69, 89, 136, 161, 177, 195)
n <- length(idx)

count.dead = list()
j <- 1
for(i in idx){
  f <- as.character(levels(iraq.dead$general.location)[idx[j]])
  tmp <- iraq.dead$Julian[iraq.dead$general.location == f]
  count.dead[[j]] <- as.data.frame(table(tmp))
    j <- j + 1
}
names(count.dead) <- 
  as.character(levels(iraq.dead$general.location)[idx])

openg(4.5, 7)
#plot(julian, rep(0, length(julian)), 
#  type = 'l', xlab = 'Date', ylab = 'Dead', 
#  lwd = 2, xaxt = 'n', ylim = c(0, 40))

#col <- c('black', 'red', 'blue', 'green', 'brown',
#  'orange', 'yellow', 'gray90', 'ivory', 'cyan', 'purple')
par(mfrow = c(11,1), mar = c(0,1,0,1) + 0.1)
for(i in 1 : length(count.dead)){
  plot(as.integer(as.character(count.dead[[i]][, 1])), 
    count.dead[[i]][, 2], xlab = '', ylab = '', 
    xaxt = 'n', yaxt = 'n', type = 'h')
  text(locator(n=1), labels = names(count.dead)[i], pos = 4)
}
  
#n <- length(iraq.daily.deaths$Dead)
#j <- as.integer(tojulian('1/1/1970')) + iraq.daily.deaths$Julian
#display.j <- as.integer(c(1, n/4, n/2, 3*n/4, n))
#labs <- fromjulian(j[display.j])
#axis(1, at = tojulian(labs)-as.integer(tojulian('1/1/1970')) , 
#labels = labs)
#
#
#lm.injured <- lm(cumsum(iraq.injured$Injured) ~ 
#  injured.julian)
#lm.dead <- lm(cumsum(count.dead[, 2]) ~ dead.julian)
#abline(reg = lm.injured) ; abline(reg = lm.dead)
#vlines <- c(12477, 12720, 13050, 13362)
#abline(v = vlines)
#d <- vector()
#for(i in 1:length(vlines)) d[i] <- tojulian('1/1/1970') + vlines[i]  
#text(locator(), labels = fromjulian(d), srt = 90)  
saveg('../docs/run-sequence-by-location-Iraq', 4.5, 7)
rm(list = ls())
library(CTFS)
load('iraq.dead.hostile.rda')
load('iraq.daily.deaths.rda')
julian <- iraq.daily.deaths$Julian
# Deadliest locations obtained from  
# levels(iraq.dead.hostile$general.location)
idx <- c(7, 42, 60, 62, 66, 69, 89, 136, 161, 177, 195)
n <- length(idx)

count.dead = list()
j <- 1
for(i in idx){
  f <- as.character(levels(iraq.dead.hostile$Where)[idx[j]])
  tmp <- iraq.dead.hostile$Julian[iraq.dead.hostile$Where == f]
  count.dead[[j]] <- as.data.frame(table(tmp))
  j <- j + 1
}
names(count.dead) <- 
  as.character(levels(iraq.dead.hostile$Where)[idx])

openg(4.5, 7.5)
par(mfrow = c(2,1), mar = c(0,4,1,1) + 0.1)

plot(julian, rep(0, length(julian)), 
  type = 'l', xlab = 'Date', ylab = 'Dead', 
  lwd = 2, lty = 1, xaxt = 'n', ylim = c(0, 250))

pos <- rep(2, length(count.dead))
pos[1] <- 4 ; pos[3] <- 4
lty <- c(1,2,3,1,2,3,1,2,3,1,2,3)
for(i in 1 : length(count.dead)){
  lines(as.integer(as.character(count.dead[[i]][, 1])), 
    cumsum(count.dead[[i]][, 2]), lty = lty[i], 
    lwd = 2)
  text(locator(n=1), labels = names(count.dead)[i], pos = 
    pos[i])
}

par(mar = c(2,4,0,1) + 0.1)

plot(julian, rep(0, length(julian)), 
  type = 'l', xlab = 'Date', ylab = 'Dead', 
  lwd = 2, lty = 1, xaxt = 'n', ylim = c(0, 1200))

for(i in 1 : length(count.dead)){
  lines(as.integer(as.character(count.dead[[i]][, 1])), 
    cumsum(count.dead[[i]][, 2]), lty = lty[i], 
    lwd = 2)
}

n <- length(iraq.daily.deaths$Dead)
j <- as.integer(tojulian('1/1/1970')) + iraq.daily.deaths$Julian
display.j <- as.integer(c(1, n/4, n/2, 3*n/4, n))
labs <- fromjulian(j[display.j])
axis(1, at = tojulian(labs), labels = labs)



  

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
saveg('../docs/cumulative-by-location-Iraq',4.5, 7.5)
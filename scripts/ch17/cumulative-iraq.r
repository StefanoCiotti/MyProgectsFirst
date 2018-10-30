rm(list = ls())
library(CTFS)
load('iraq.dead.hostile.rda')
load('iraq.injured.rda')
load('iraq.daily.deaths.rda')

count.dead <- as.data.frame(table(iraq.dead.hostile$Julian))
dead.julian <- as.integer(as.character(count.dead[, 1]))
injured.julian <- as.integer(iraq.injured$Julian)

# cumulative
openg(4.5)
plot(injured.julian, cumsum(iraq.injured$Injured), 
  xlab = 'Date', ylab = 'Dead or Injured', 
  xaxt = 'n',  ylim = c(0, 30000))
points(dead.julian, cumsum(count.dead[, 2]))
#abline(h = c(0, 5000, 10000, 15000, 20000, 25000))
  
n <- length(iraq.daily.deaths$Count)
j <- iraq.daily.deaths$Julian
display.j <- as.integer(c(1, n/4, n/2, 3*n/4, n))
labs <- fromjulian(j[display.j])
axis(1, at = tojulian(labs), labels = labs)

lm.injured <- lm(cumsum(iraq.injured$Injured) ~ 
  injured.julian)
lm.dead <- lm(cumsum(count.dead[, 2]) ~ dead.julian)
abline(reg = lm.injured) ; abline(reg = lm.dead)

#locations <- identify(injured.julian, 
#  cumsum(iraq.injured$Injured), plot = FALSE)
#vlines <- c(injured.julian[locations])
#abline(v = vlines)
#d <- vector()
#for(i in 1:length(vlines)) d[i] <- tojulian('1/1/1970') + vlines[i]  
#text(locator(), labels = fromjulian(d), srt = 90)  
saveg('../docs/cumulative-Iraq', 4.5)

# residuals
openg(4.5, 4.5)
par(mfrow = c(2, 1), mar = c(0,4,0,1) + 0.1)
plot(lm.injured$residuals, , xlab = '', 
  ylab = 'Residuals, injured', xaxt = 'n')
abline(v = c(13, 21, 41))
abline(h = 0)
par(mar = c(6,4,0,1) + 0.1)
plot(lm.dead$residuals, xaxt = 'n', xlab = '',
  ylab = 'Residuals, dead')
abline(h = 0)
periods <- c(180, 372, 806)
abline(v = periods)
periods <- c(1, periods, length(dead.julian))
labs <- fromjulian(dead.julian[periods])
axis(1, at = periods, labels = labs, las = 2, xlab = '')
saveg('../docs/cumulative-Iraq-residuals', 4.5, 4.5)

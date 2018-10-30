rm(list = ls())
library(CTFS)
load('iraq.injured.rda')
load('iraq.daily.deaths.rda')

openg(4.5, 4.5)
par(mfrow = c(2, 1), mar = c(1,4,1,1) + 0.1)
# injuries by month
plot(iraq.injured$Julian, iraq.injured$Injured, type = 'h',
  ylab = 'Monthly injuries', xlab = '', xaxt = 'n')
abline(h = mean(iraq.injured$Injured))
#x <- as.integer(iraq.injured$Julian)
#model <- lm(iraq.injured$Injured ~ x)
#abline(reg = model)

# dead by day
par(mar = c(4,4,0,1) + 0.1)
plot(iraq.daily.deaths$Julian,iraq.daily.deaths$Count,
  type = 'h', ylab = 'Daily mortality',
  xlab = 'Day', xaxt = 'n')
abline(h = mean(iraq.daily.deaths$Count))

n <- length(iraq.daily.deaths$Count)
j <- iraq.daily.deaths$Julian
display.j <- as.integer(c(1, n/4, n/2, 3*n/4, n))
labs <- fromjulian(j[display.j])
axis(1, at = tojulian(labs) , labels = labs, srt = -90)
saveg('../docs/run-sequence-Iraq',4.5,4.5)

#h(iraq.injured$Injured)
#
#
#
#
#
#
#d <- iraq.daily.deaths
#(b <- ceiling((d$Julian[length(d$Julian)] - d$Julian[1]) / 7))
#cj <- cut(d$Julian, b)
#counts <- table(cj)
#
#plot(counts, axes = FALSE,
#  xlab = 'deaths per week',
#  ylab = 'counts', type = 'h')
#axis(2)
#abline(h = mean(counts))
#
#n <- length(iraq[, 1])
#labs <- as.character(c(iraq[1, 2],
#  iraq[as.integer(length(iraq[, 2]) /2), 2],
#  iraq[length(iraq[, 2]), 2]))
#axis(1, at = c(1, round(length(counts) / 2, 0),
#  length(counts)), labels = labs, srt = -90)
#
#n <- length(counts)
#x <- 1 : n
#periods <- n * 10 / 365
#start <- 140
#a <- 4
#y <- mean(sqrt(counts)) + a * sin((x - start) / (periods))
#
##lines(y, lwd = 3)
#
#openg()
#h(counts)
#x <- 0 : 90
#mu <- mean(counts); size <- mu^2/(var(counts) - mu)
#lines(x, dnbinom(x, size, , mu))
#
##library(evd)
##
##m <- mean(counts); a <-  4; start <- 140 ; 
##counts.nls <- nls(counts ~ fo(x, m, a, start, periods), 
##   start = list(a = a, b = b, c = c, d = d))
##YM.co <- c(mean = m, coefficients(YM.nls))
##summary(YM.nls)
##
#
##plot(y - as.vector(counts))
rm(list = ls())
load('iraq.dead.hostile.rda')

d <- iraq.dead.hostile
where.julian <- as.data.frame(table(d$Where,d$Julian))
names(where.julian) <- c('Where', 'Julian', 'Count')
where.julian <- where.julian[where.julian$Count != 0, ]
# now we have count for a unique pair of location-julian
# because deaths are reported by a person, Count
# is the number of dead for each unique pair location-julian

# how many different locations reported on different julians:
d.l <- as.data.frame(table(where.julian$Where))
names(d.l) <- c('Where', 'Count')

# a table on where.julian$Julian tells you how many different
# locations for a single Julian
events <- as.data.frame(table(where.julian$Julian))
names(events) <- c('Julian', 'Events')

counts <- where.julian$Count

# dead, negative binomial - DOES NOT FIT WELL
#dead.h <- h(counts, xlab = 'Dead')
#x <- 0:25
#mu <- mean(counts); 
#size <- mu^2/(var(counts) - mu)
#lines(x, dnbinom(x, size, , mu))
#
#x <- dead.h$counts
#y <- dnbinom(dead.h$mids, size, , mu)
#xy <- cbind(x, y)
#cst <- chisq.test(xy, correct = FALSE,
#  simulate.p.value = TRUE, B = 2000)
#print(cst)

#dead, Poisson - all places
openg()
breaks <- seq(0, 30, by = 1)
dead.h <- h(counts, xlab = 'Dead', ylab = 'Density', breaks = breaks,
  xlim = c(0, 15), plot = FALSE)
x <- breaks[-1]
mu <- mean(counts)
plot(dead.h$breaks[-1], dead.h$intensities, pch = 19, 
  cex = 1.5, xlab = 'Dead', ylab = 'Probability')
lines(x, dpois(x, mu), type = 'h', lwd = 2)
abline(h = 0)

x <- dead.h$density
y <- dpois(breaks, mu)
xy <- cbind(x, y[-1])
cst <- chisq.test(xy, correct = FALSE, 
  simulate.p.value = TRUE, B = 2000)
print(cst)
saveg('../docs/Iraq-multiple-attacks-on-the-same-day')


# dead, Poisson for places with frequent attacks
threshold <- 50 # reported attacks
v <- table(where.julian$Where)
v <- v[v > threshold]
par(mfrow = c(2, 4))
dead.h <- list()
for(i in 1 : length(v)){
  counts <- where.julian$Count[where.julian$Where ==
    dimnames(v)[[1]][i]]
  dead.h[[i]] <- h(counts, xlab = 'Dead')
  x <- dead.h[[i]]$breaks
  mu <- mean(counts); 
  lines(x, dpois(x, mu), type = 'h', lwd = 2)
  
  x <- dead.h[[i]]$mids
  y <- dpois(dead.h[[i]]$mids, mu)
  xy <- cbind(x, y)
  cst <- chisq.test(xy, correct = FALSE,
    simulate.p.value = TRUE, B = 2000)
  print(cst)
}  


#http://icasualties.org/oif/
rm(list = ls())
casualties <- read.table('Iraq-casualties.txt', sep = '\t')
casualties$V1 <- as.Date(casualties$V1, '%m/%d/%Y')
head(casualties, 5)

casualties$V1 <- sort(casualties$V1)
jd <- julian(casualties$V1)
casualties <- data.frame(Date = casualties$V1, Julian = jd)
save(casualties, file = 'casualties.rda')
(b <- ceiling((jd[length(jd)] - jd[1]) / 10))
(cnts <- table(cut(jd, b)))
save(cnts,file = 'Iraq.cnts.rda')
plot(cnts, axes = FALSE, xlab = 'date of 10-day interval',
  ylab = 'casualties')
axis(2)
n <- length(casualties[, 1])
labs <- as.character(c(casualties[1, 1],
  casualties[as.integer(length(casualties[, 1]) /2), 1],
  casualties[length(casualties[, 1]), 1]))
axis(1, at = c(1, round(length(cnts) / 2, 0),
  length(cnts)), labels = labs)



h(cnts, xlab = 'deaths per 10-day intervals',
  ylim = c(0, 0.04))
c.h <- hist(cnts, xlab = 'deaths per 10-day intervals',
  ylim = c(0, 0.04), freq = FALSE, main = '')
x <- 0 : 100
m <- mean(cnts) ; v <- var(cnts)
sz <- m^2/(v-m)
lines(x, dnbinom(x, size = sz, mu = m), lwd = 2,
  lty = 2, col = 'red')
ks.test(c.h$density, dnbinom(c.h$mids, size = sz, mu = m))
chisq.test(c.h$density,
  y = dnbinom(c.h$mids, size = sz, mu = m))

rm(list = ls())
openg(4.5, 3.5)
par(mfrow = c(1, 2))
load('college.crime.rda')
c.c <- college.crime[, c('school', 'city', 'state',
  'enrollment', 'full-time faculty')]
condition <- (c.c[, 4] >= 2520 & c.c[, 4] <= 56348) &
   (c.c[, 5] >= 1 & c.c[, 5] <= 10378) 
c.c <- c.c[condition, ]
r <- c.c[, 'enrollment'] / c.c[, 'full-time faculty']
log.r <- log(r)

h(log.r, xlab = 'log(enrollment / faculty)')
x <- seq(-1, 5, length = 201)
lines(x, dnorm(x, mean(log.r), sd(log.r)))
print(mean(log.r)) ; print(shapiro.test(log.r))


plot(r, ylab = 'ratio')  # plot the ratio
idx <- identify(r) # ideintify bad ratios
(bad <- cbind(c.c[idx, 1 : 4], ratio = round(r[idx], 1)))
saveg('student-faculty-ratio', 4.5, 3.5)

openg()
qqnorm(log.r, main = 'log(enrollment / faculty)')
qqline(log.r)
saveg('student-faculty-ratio-QQ')

round(c( '0%' = mean(r), 
   '10%' = mean(r, trim = 0.1),
   '20%' = mean(r, trim = 0.2)), 1)


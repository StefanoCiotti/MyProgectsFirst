tbr <- read.csv('TeenBirthRate2002.txt', 
   sep = '\t')
head(tbr, 5)

Average<-vector()
for(i in 1:length(tbr[,1]))
   Average[i] <- round(mean(t(tbr[i, 2:4])),2)
tbr <- data.frame(tbr, Average)
head(tbr, 5)
save(tbr,file='TeenBirthRate.rda')

openg(4.5, 4.5)
par(mfrow = c(2, 2))
xlim <- c(0, 160)
main <- c('White', 'Black', 'Hispanic', 'Average')
xlab <- c('', '', 'birth rate (per 1000)',
   'birth rate (per 1000)')
ylab <- c('density', '', 'density', '')
for(i in 1:4)
   hist(tbr[, i+1], freq = FALSE,
      xlab = xlab[i], xlim = xlim,
      ylab = ylab[i], main = main[i])
saveg('teen-birth-rate', 4.5, 4.5)

X.bar <- mean(tbr[, 3 : 4], na.rm = TRUE) ; round(X.bar, 2)
mu <- mean(tbr[, 2], na.rm = TRUE)
sigma <- sd(tbr[, 2], na.rm = TRUE)
round(c(mu, sigma), 2)
Z <- (X.bar - mu) / sigma ; round(Z, 3)

round(pnorm(Z, mu, sigma), 4)

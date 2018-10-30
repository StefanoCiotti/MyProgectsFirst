rm(list = ls())
options(width = 60)

birthwt<-read.table('low-birth-wt.txt',
  header = TRUE, sep = '\t')
names(birthwt) <- tolower(names(birthwt))
attach(birthwt)

race <- factor(race, labels = c('white', 'black', 'other'))
ptd <- factor(ptl > 0)
ftv <- factor(ftv)
levels(ftv)[-(1 :2 )] <- '2+'
bwt<-data.frame(low = factor(low), age, lwt, race,
   smoke = (smoke > 0), ptd, ht=(ht > 0), ui = (ui > 0), ftv)
save(bwt, file = 'bwt.rda')

attach(bwt)
print(table(low, smoke))
print(table(low, smoke, race))

X <- smoke
Y <- low
library(Design)
ddist <- datadist(X, Y)
options(datadist = 'ddist')
(model <- lrm(Y ~ X, x = TRUE, y = TRUE))

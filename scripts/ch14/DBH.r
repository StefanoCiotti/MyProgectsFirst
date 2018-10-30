rm(list = ls())
dbh <- read.table('DBH.txt', header = FALSE, sep = '\t')
names(dbh) <- c('DBH', 'height', 'age')
h <- hist(dbh$age, breaks = 10, plot = FALSE)
age.categ <- cut(dbh$age, br = h$breaks, 
   include.lowest = TRUE)
dbh <- data.frame(dbh, age.categ)
names(dbh)[4] <- 'age categ'

DBH <- tapply(dbh$DBH, dbh$'age categ', mean)
age.center <- h$mids
n <- h$counts
dbh.categ <- data.frame(DBH, age.center, n)
names(dbh.categ)[1] <- 'mean DBH'

par(mfrow = c(1, 2))
plot(dbh$age, dbh$DBH, xlab = 'age (years)', 
   ylab = 'DBH (cm)')
plot(dbh.categ$age[1 : 4], (dbh.categ$'mean DBH'[1 : 4])^2, 
   xlab = 'age (years)', ylab = expression(DBH^2), 
   ylim = c(1, (150)^2))
lines(dbh.categ$age, (dbh.categ$'mean DBH')^2)
dbh.split <- split((dbh$DBH)^2, dbh$'age categ')

for(i in 1 : 4){
   x <- rep(dbh.categ$age[i], dbh.categ$n[i])
   y <- dbh.split[[i]]
   points(x, y)
   if(i == 3) identify(x, y, labels = round(y))
}

x <- dbh.categ$age[1 : 4]
y <- (dbh.categ$'mean DBH'[1 : 4])^2
dbh.lm <- lm(y~x)
abline(reg = dbh.lm, lwd = 3)
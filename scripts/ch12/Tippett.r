Tippett <- read.table('Tippett.txt', header = TRUE)
names(Tippett) <- c('meadow pipit', 'wren')

par(mfrow = c(1, 2))
hist(Tippett[, 1], main = '', xlab = names(Tippett)[1], 
   ylab = 'density')
hist(Tippett[, 2], main = '', 
 xlab = names(Tippett)[2], ylab = '')

round(c(var(Tippett[, 1]), var(Tippett[, 2])), 3)

wilcox.test(Tippett[, 1], Tippett[, 2])

t.test(Tippett[, 1], Tippett[, 2])

e <- read.csv('elections2000.csv')
barplot(sapply(e[, 2 : 5] / 1000, sum), las = 2,
   main = 'elections 2000, Florida', ylab = 'in thousands',
   col = 'gray90')

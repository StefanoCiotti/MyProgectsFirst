rm(list = ls())
library(SuppDists)
bill.length <- c(2.50, 2.83, 2.95, 3.24, 3.32, 3.43, 3.60, 
   3.82, 4.00, 4.40)
score <- normOrder(10)
plot(score, bill.length) ; r = lm(bill.length~score)
abline(reg = r)
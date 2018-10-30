rm(list = ls())
x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20)
set.seed(111) ; Y <- x^2 + rnorm(length(x), 0, 10)
# openg()
plot(x, Y)
# saveg('deceiving-R')
Pearson <- cor.test(x, Y)[[4]]
Spearman <- cor.test(x, Y, method = 'spearman')[[4]]
round(c(Pearson = Pearson, Spearman = Spearman), 2)
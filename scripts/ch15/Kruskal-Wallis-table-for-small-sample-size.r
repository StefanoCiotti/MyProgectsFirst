library(SuppDists)

(p.value <- 1 - round(pKruskalWallis(4.668, 3, 13,
  sum(c(1/4, 1/4, 1/5))),2))










age = c(rep('young of the year', 9), rep('juvenile', 9),
  rep('adult', 9))
stocking = rep(c(rep('low', 3), rep('medium', 3), rep('high', 3)), 3)
set.seed(101)
stocking <- data.frame(cbind(age, stocking, 'weight gain' = sample(c('low', 'medium', 'high'), length(age), replace = TRUE)))
save(stocking, file = 'stocking.rda')

(stocking[, 1] <- factor(stocking[, 1], levels = c('young of the year', 'juvenile', 'adult'), ordered = TRUE))
(stocking[, 2] <- factor(stocking[, 2], levels = c('low', 'medium', 'high'), ordered = TRUE))
(stocking[, 3] <- factor(stocking[, 3], levels = c('low', 'medium', 'high'), ordered = TRUE))

library(pgirmess)
k <- kruskalmc(stocking[c(1 : 9, 10, 13, 16), 3], stocking[c(1 : 9, 10, 13, 16), 2])
summary(k)
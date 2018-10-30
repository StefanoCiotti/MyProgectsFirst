rm(list = ls())
load('SO2.rda')
names(SO2)

(n <- tapply(SO2[, 2], SO2[, 3], length))
(Total.mean <- mean(SO2[, 2]))

(Group.means <- tapply(SO2[, 2], SO2[, 3], mean))
(SO2 <- data.frame(SO2, Group.means = c(
  rep(Group.means[1], n[1]), rep(Group.means[2], n[2]),
  rep(Group.means[3], n[3]))))

(Total.SS <- sum((SO2[, 2] - Total.mean)^2))

(Within.SS <- sum(tapply((SO2[, 2] - SO2[, 4])^2,
  SO2[, 3], sum)))
(Between.SS <- sum(tapply((SO2[, 4] - Total.mean)^2,
  SO2[, 3], sum)))

(Within.MS <- Within.SS / (sum(n) - length(n)))
(Between.MS <- Between.SS / (length(n) - 1))
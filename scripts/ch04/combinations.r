library(combinat)
nc <- nCm(5, 3)
np <- nCm(5, 3) * fact(3)
print(c(comb = nc, perm = np))

x <- LETTERS[1 : 5]
nqd(combn(x, 3))

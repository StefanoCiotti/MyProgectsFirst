load('fish.rda')
options(width = 60)
idx <- sample(dimnames(fish$adults)[[1]], 10)
fish$adults[idx, c(1, 4 : 6, 8 : 10)]
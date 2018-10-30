rm(list = ls())
load('EU.CO.rda')

(tb <- table(EU.CO[, c(3, 4)]))

length(Y.3.2 <- EU.CO[EU.CO[, 3] == 'GERMANY' &
  EU.CO[, 4] == 'suburban', 2])

Y.3.2[1]

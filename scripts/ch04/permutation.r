nqd <- function(x) print(noquote(no.dimnames(x)))

x <- letters[23 : 26]
nqd(array(x))

library(combinat)
px <- unlist(permn(x))
nqd(array(px[1 : 10]))

pmx <- matrix(px, ncol=4, byrow=T)

pmx <- cbind(pmx[1 : 6, ], '   ', pmx[7 : 12, ],
  '   ', pmx[13 : 18, ], '   ', pmx[19 : 24, ])
nqd(pmx)

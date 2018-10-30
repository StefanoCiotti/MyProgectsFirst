rm(list = ls())
load('bm.rda')
load('bmv.rda')

trimmed <- apply(as.array(bmv[, 2]), 1, function(x)
  if (substr(x,   nchar(x), nchar(x)) == ' ')
  strtrim(x, nchar(x) - 1) else strtrim(x, nchar(x)))
bmv[, 2] <- unlist(trimmed)
head(bmv, 4)

trimmed <- apply(as.array(as.character(bmv[, 1])), 1,
  function(x)
  if (substr(x,   nchar(x), nchar(x)) == ' ')
  strtrim(x, nchar(x) - 1) else strtrim(x, nchar(x)))
bmv[, 1] <- unlist(trimmed)


openg(4, 4)
pairs(bm[, c(3, 9, 16)],
  labels = bmv[c(3, 9, 16), 2])
#saveg('body-measurements', 4, 4)




rm(list = ls())
load('bm.rda')
load('bmv.rda')

trimmed <- apply(as.array(as.character(bmv[, 1])), 1,
  function(x)
  if (substr(x,   nchar(x), nchar(x)) == ' ')
  strtrim(x, nchar(x) - 1) else strtrim(x, nchar(x)))
bmv[, 1] <- unlist(trimmed)
head(bmv, 4)





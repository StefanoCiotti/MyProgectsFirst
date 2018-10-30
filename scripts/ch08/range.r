(xy <- list(x = c(0, rep(100, 4)),
  y = c(100, 140, 180, 200, 100)))
mapply(range,xy)
mapply(var, xy)

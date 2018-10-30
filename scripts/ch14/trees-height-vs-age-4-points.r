rm(list = ls())
load('dbh.rda')
attach(dbh)
id <- c(306, 79, 58, 301)
x <- log(age[id]) ; y <- log(height[id])
model <- lm(y ~ x)
ey <- predict(model)

plot(x, y, xlim = c(1, 5), ylim = c(0, 3.5), 
  xlab = 'log(age)', ylab = 'log(height)')
abline(reg = model)
for(i in 1 : length(x))
  lines(c(x[i], x[i]), c(y[i], ey[i]))
points(x, ey, pch = 19)
text(x, ey, labels = round(ey, 3), pos = 4)
text(x, y, labels = round(y, 3), pos = 4)
for (i in 1 : length(x)){
  if (i == 2 | i == 4) pos = 2 else pos = 4
  text(x[i], ey[i] - (ey[i] - y[i]) / 2, 
    labels = bquote(epsilon[.(i)] == 
    .(round(y[i] - ey[i], 3))), pos = pos)
}

for(i in 1 : length(log.X)){
   if(i == 2 | i == 4) pos = 2 else pos = 4
   text(log.X[i], model$fitted.values[i] -
    (model$fitted.values[i] - log.Y[i]) / 2,
      labels=bquote(epsilon[.(i)]==.(round(log.Y[i] -
      model$fitted.values[i], 3))), pos = pos)
}


d <- data.frame(x, y, ey, y - ey, (y - ey)^2)
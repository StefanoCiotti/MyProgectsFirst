rm(list=ls())
x <- seq(1, 10, length = 21)

ss <- function(i){
  r <- c(0.8, 2.5, 0.8, 2.5)
  b0 <- 1
  b1 <- c(1, 1, .5, .5)
  set.seed(i + 1)
  Y <- b0 + b1[i] * x + rnorm(length(x), 0, r[i])
  adj <- mean(Y) - 5
  model <- lm(Y ~ x)
  m <- c('large reg SS, small res SS',
    'large reg SS, large res SS',
    'small reg SS, small res SS',
    'small reg SS, large res SS')
  xlab <- '' ; ylab <- ''
  if (i == 1 | i == 3) ylab = 'Y'
  if (i == 3 | i == 4) xlab = 'x'
  plot(x, Y, ylim = c(0, 12), main = m[i],
    xlab = xlab, ylab = ylab)
  abline(reg = model)
  abline(h = mean(Y))
  RegSS <- sum((predict(model) - mean(Y))^2)
  ResSS <- sum((Y - predict(model))^2)
  cbind('regression SS' = RegSS, 'residual SS' = ResSS,
     'total SS' = RegSS + ResSS, Y = mean(Y))
}

openg(4.5, 4.5)
par(mfrow = c(2, 2))
b0 <- 1
SS <- matrix(ncol = 4, nrow = 4)
for (i in 1 : 4) ss(i)
saveg('goodness-of-fit', 4.5, 4.5)
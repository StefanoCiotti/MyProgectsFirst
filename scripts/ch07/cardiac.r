library(foreign) ; cardiac <- read.dta('cardiac.dta')
h(cardiac$basebp, xlab = 'base blood pressure')
x <- seq(80, 220, length = 201)
mu <- mean(cardiac$basebp)
sigma <- sqrt(sum((cardiac$basebp - mu)^2) /
  length(cardiac$basebp))
lines(x, dnorm(x, mu, sigma))
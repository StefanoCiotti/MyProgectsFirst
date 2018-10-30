set.seed(1) ; y <- rnorm(1000, 18, 6)
h(y, xlab = 'animals at the watering hole')
x <- seq(0, 40, length = 1001) ; lines(x, dnorm(x, 18, 6))
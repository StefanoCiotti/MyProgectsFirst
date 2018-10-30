x <- 50 : 1 ; 
y <- x + runif(50, min = 10, max = 20)
windows(width = 3, height = 3, pointsize = 8) ; 
plot(x, y)
abline(lm(y~x))

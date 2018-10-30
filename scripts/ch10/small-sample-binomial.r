library(Hmisc)
n<-40 ; nS <-17 ; pi0 <- 5.264/(5.265+4.401) ; 
p <-nS / n ; EnS <- n * pi0
round(binconf(EnS , n , 2 * alpha) , 3)

z <- qnorm(alpha)
a <- (2 * EnS + z^2) / (2 * (n + z^2))
b <- z * sqrt(n * (4 * EnS + z^2) - 4 * EnS^2) / 
(2 * sqrt(n) * (n + z^2))
critical.value <- min(a + b , a - b)
round(c(p , critical.value ) , 3)

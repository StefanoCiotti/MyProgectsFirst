rm(list = ls())

n <- 20 ; set.seed(102) 
Weight <- rnorm(20, 25, 5) ; metabolic.rate <- Weight^0.75
CV <- vector()
for(i in 1 : 10000){
  S <- sample(metabolic.rate, n, replace = TRUE)
  CV[i] <- sd(S) / mean(S) * 100
}
mean.CV <- mean(CV) ; sd.CV <- sd(CV)
h(CV, xlab = 'CV', xlim = c(5, 25), ylim = c(0, 0.2))
x <- seq(5, 25, length = 201)
lines(x, dnorm(x, mean.CV, sd.CV))
CI <- quantile(CV, probs = c(0.025, 0.975))
abline(v = CI[1]) ; abline(v = CI[2])
abline(v = mean.CV, lwd = 2, col = 'red')

round(c(mean = mean.CV, CI), 3)

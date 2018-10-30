par(mfrow = c(3, 2)) ; set.seed(5) ; x<- rexp(100, .5)
xx <- list(x = x, 'log(x)' = log(x), 'sqrt(x)' = sqrt(x))
xlabels <- c('x', 'log(x)', 'sqrt(x)')
for(i in 1 : 3){
  h(xx[[i]], xlab = xlabels[i])
  qqnorm(xx[[i]], main = '') ; qqline(xx[[i]])
}

test <- mapply(shapiro.test, xx)
rbind(x = unlist(test[1 : 2, 1]),
  'log(x)' = unlist(test[1 : 2, 2]),
  'sqrt(x)' = unlist(test[1 : 2, 3]))
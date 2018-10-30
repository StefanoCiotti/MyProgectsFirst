Bonferroni <- function(x, Within.MS, alpha = 0.05){
  # x - a two column data frame (no NA)
  #  x[, 1] is a factor; x[, 2] is numeric
  n <- as.numeric(tapply(x[, 2], x[, 1], length))
  k <- length(n)
  df <- sum(n) - k
  means <- as.numeric(tapply(x[, 2], x[, 1], mean))
  a <- aov(x[, 2] ~ x[, 1])
  Within.MS <- sum(a$residuals^2) / a$df.residual
  x.split <- split(x[, 2], x[, 1])
  names(x.split) <- levels(x[, 1])
  results <- data.frame(x.1 = NA, x.2 = NA, x.3 = NA, x.4 = NA,
    x.5 = NA, x.6 = NA)
  for(i in 1 : (k - 1)){
    for(j in (i + 1) : k){
      SE <- as.numeric(sqrt(Within.MS * (1 / n[i] + 1 / n[j])))
      alpha.1 <- alpha / choose(k, 2)
      Tij <- as.numeric((means[i] - means[j]) / SE)
      p.value <- 1 - pt(abs(Tij), df)
      results <- rbind(results, c(i, j, Tij, df, p.value, alpha.1))
    }
  }
  results <- round(results[-1, ], 4)
  dimnames(results) <- list('rows' = 1 : length(results[, 1]),
    'cols' = c('i', 'j', 'T', 'df', 'p-value', 'alpha'))
  results
}
      

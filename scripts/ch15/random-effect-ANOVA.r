re.1w <- function(group, repl, response){
  n.i <- length(unique(repl))
  k <- length(unique(group))
  n <- n.i * k
  Y.bar.i <- tapply(response, group, mean)
  case.means <- rep(Y.bar.i, n.i)
  Y.bar.bar <- mean(response)
  Between.SS <- sum(n.i * (Y.bar.i - Y.bar.bar)^2)
  Between.MS <- Between.SS/(k - 1)
  Within.SS <- sum(tapply((response - case.means)^2,
    group, sum))
  Within.MS <- Within.SS / (n - k)
  F <- Between.MS / Within.MS
  p.value <- 1 - pf(F, k - 1, n - k)
  source <- c('Between.MS (model)', 'Within.MS (error)')
  df <- c(k - 1, n - k)
  Mean.SS <- round(c(Between.MS, Within.MS), 3)
  F <- c(round(F, 3), ' ')
  p.value <- c(round(p.value, 3), ' ')
  data.frame(source, df, Mean.SS, F, p.value)
}
bp <-
function (p1, p2, odds.ratio, percent.reduction, n, n1, n2,
  alpha = 0.05, alt = c('two.sided', 'greater', 'less'))
{
  # From the Harel's Hmisc package, with one-sided added

  if (!missing(odds.ratio))
      p2 <- p1 * odds.ratio/(1 - p1 + p1 * odds.ratio)
  else if (!missing(percent.reduction))
      p2 <- p1 * (1 - percent.reduction/100)
  if (!missing(n)) {
      n1 <- n2 <- n/2
  }
  if(alt == 'two.sided'){
    z <- qnorm(1 - alpha/2)
  }
  else{
    z <- qnorm(1 - alpha)
  }
  q1 <- 1 - p1
  q2 <- 1 - p2
  pm <- (n1 * p1 + n2 * p2)/(n1 + n2)
  ds <- z * sqrt((1/n1 + 1/n2) * pm * (1 - pm))
  ex <- abs(p1 - p2)
  sd <- sqrt(p1 * q1/n1 + p2 * q2/n2)
  if(alt == 'two.sided'){
    c(Power = 1 - pnorm((ds - ex)/sd) + pnorm((-ds - ex)/sd))
  }
  else if(alt == 'greater'){
    c(Power = 1 - pnorm((ds - ex)/sd))
  }
  else{
    c(Power = pnorm((-ds - ex)/sd))
  }
}
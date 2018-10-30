load('trisomy.table.rda')
m.j <- trisomy.table$total
x.j <- trisomy.table$coded
n.j <- trisomy.table$trisomic
beta.0 <-  -1.254 ; beta.1 <- 0.347
coef <- c(beta.0, beta.1) 
n.hat.j <- m.j * (1 / (1 + exp(-(beta.0 + beta.1 * x.j))))
r.j <- (n.j - n.hat.j) / 
  (sqrt(n.hat.j * (m.j - n.hat.j) / m.j))
pearson.chi.sq <- sum(r.j^2)
d.j <- sqrt(2 * (n.j * log(n.j / n.hat.j) + 
   (m.j - n.j) * log((m.j - n.j) / (m.j - n.hat.j))))
deviance.chi.sq <- sum(d.j^2)
df <- length(m.j) - length(coef)
pearson.p.value <- 1 - pchisq(pearson.chi.sq, df)
deviance.p.value <- 1 - pchisq(deviance.chi.sq, df)
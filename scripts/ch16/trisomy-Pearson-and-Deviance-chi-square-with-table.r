load('trisomy.table.rda')
m.j <- trisomy.table$total
x.j <- trisomy.table$coded
n.j <- trisomy.table$trisomic
beta0<-  -1.254; beta1<- 0.347
coef <- c(beta0, beta1) 
n.hat.j <- m.j * (1 / (1 + exp(-(beta0 + beta1 * x.j))))
r.j <- (n.j - n.hat.j) / (sqrt(n.hat.j * (m.j - n.hat.j) / m.j))
pearson.chi.sq <- sum(r.j^2)

d.j <- sqrt(2 * (n.j * log(n.j / n.hat.j) + 
   (m.j - n.j) * log((m.j - n.j) / (m.j - n.hat.j))))
deviance.chi.sq <- sum(d.j^2)

df <- length(m.j) - length(coef)
pearson.p.value <- 1 - pchisq(pearson.chi.sq, df)
deviance.p.value <- 1 - pchisq(deviance.chi.sq, df)

pearson.deviance.res <- cbind(m.j, n.j, n.hat.j, r.j, d.j)
pearson.deviance.res <- rbind(pearson.deviance.res,
   c(NA, NA, NA, 
   pearson.chi.sq, deviance.chi.sq))
pearson.deviance.res <- rbind(pearson.deviance.res, 
   c(NA, NA, NA, pearson.p.value, deviance.p.value))

dimnames(pearson.deviance.res)[[2]] <- 
  c('~~~$m_j$~~~', '~~~$n_j~~~$', 
   '~~~~~$\\widehat{n}_j$~~~~~',
   'Pearson', 'Deviance')
dimnames(pearson.deviance.res)[[1]] <- 
  c(x.j,'$\\chi^2_4$','$p$-value')

library(Hmisc)
latex(pearson.deviance.res,
   file='trisomy-Pearson-and-Deviance-chi-sqWithTable.tex', 
   label = 'table: Pearson and deviance chi', 
   cdec = c(0, 0, 3, 3, 3), 
   col.just = rep('r', 5), 
   rowlabel.just="r",
   rowlabel = '~~~$\\bm{X}_j$~~~', 
   caption = 'Pearson and deviance $\\chi^2$ residuals.', 
   na.blank = TRUE, 
   landscape = FALSE, 
   where = "!htbp", 
   ctable = TRUE)

library(SADR) ; data(TeenBirthRate) 
attach(TeenBirthRate)

x <- c(Black, Hispanic, White)
m <- length(White)
col <- c(rep('Black', m), rep('Hispanic', m), 
   rep('White', m))
lambda<-mean(x,na.rm=TRUE)
xbar <- tapply(x, col, mean, na.rm = TRUE)
s <- sqrt(xbar)
n <- c(length(x[!is.na(x)&col == 'Black']), 
   length(x[!is.na(x)&col == 'Hispanic']), 
   length(x[!is.na(x)&col == 'White']))
stats <- cbind(xbar, s, n)

z <- function(x) (x[1] - x[4]) / (x[2]/ sqrt(x[3]))
z.test <- vector()
for (i in 1 : 3) z.test <- c(z.test, z(c(stats[i,], 
   lambda)))
stats <- cbind(stats, z.test, p=ifelse(z.test<0,
   pnorm(z.test), 1 - pnorm(z.test)))
print(zapsmall(stats,4))

library(Hmisc)
dimnames(stats)[[2]]<-c('$\\overline{X}$','$S^a$','$n^b$','$Z$','$p^c$')
latex(zapsmall(stats,4),
   title='teen-birth-Poisson',
   label='table: teen-birth-Poisson',
   col.just=rep('r',5),
   rowlabel='', 
   caption='Teen birth rates under the assumption of Poisson distribution.',
   insert.bottom='$^a$ $S$ $=$ $\\sqrt{\\overline{X}}$ \\newline $^b$ Based on states data. \\newline $^c$ $p$ is the area under the normal that gives the probability of getting the vlaue $Z$ or larger.',
   na.blank=TRUE,
   landscape=FALSE,
   where="!htbp",
   ctable=TRUE)




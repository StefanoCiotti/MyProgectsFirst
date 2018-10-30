rm(list = ls())
library(Hmisc) ; library(clim.pact)
source('bootstrap.R')

# prepare the data
load('college.crime.rda')
cc <- college.crime
sex <- 65 ; cc[cc[, sex] > 90, sex] <- NA
assaults <- cc[, sex]
enrollment <- cc$enrollment
lambda.hat <- assaults / enrollment * 1000

par(mfrow = c(1, 2))
plot(lambda.hat, ylab = 'assualts')
abline(h = 1)
ID <- identify(lambda.hat)

School <- lower.case(cc[ID, 5])
City <- lower.case(cc[ID, 6])
State <- cc[ID, 7]
tops <- data.frame(ID, School, City, 
   State, Assaults = lambda.hat[ID])
idx <- sort(tops$Assaults, decreasing = TRUE, 
  index.return = TRUE)
(tops <- tops[idx$ix, ])

#c1 <- 'Highest incidences (per 1000 enrolled '
#c2 <- 'students) of reported sexual assaults in 1994 '
#c3 <- 'among 680 US univresities and colleges.'
#latex(collegeSexualAssaults, 
#   label = 'table: collegeSexualAssaults', 
#   cdec = c(0, 0, 0, 0, 3), 
#   col.just = c('r', 'l', 'l', 'l', 'c'), 
#   extracolheads = c('', '', '', '', 'per 1000'), 
#   rowname = '', rowlabel = '', caption = paste(c1, c2, c3), 
#   where = "!htbp", ctable = TRUE)


# EDA
h(lambda.hat, xlab = 'sexual assault rate')

# bootstrap   
n <- 25 ; set.seed(1); 
idx <- sample(1 : length(assaults), n)
r <- data.frame(a = assaults[idx], e = enrollment[idx])
hist(r$a / r$e, main = expression(paste('sample, ', 
   italic(n == 25))), freq = FALSE, ylab = 'density', 
   xlab = 'assaults / enrollment', col = 'grey90')

# bootstrap the sample
a.e.ratio <- function(r) sum(r$a) / sum(r$e)
r.boot <- bootstrap(r, a.e.ratio, B = 10000)
print(summary(r.boot))
plot(r.boot, main = '', ylab = 'density', col = 'gray90')
qqnorm(r.boot)
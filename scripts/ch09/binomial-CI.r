library(Hmisc) ; 
set.seed(1)
nS <- as.integer(runif(5, 10, 20)) ; 
set.seed(2)
n <- as.integer(runif(5, 40, 60))
cnames <- c('$n$', '$n_{S}$', 'Estimate of $\\pi$', 
   'Lower', 'Upper')
rnames <- rep(' ', 5)
binomialCI <- matrix(cbind(n, nS, binconf(nS, n)), 
      ncol = 5, nrow = 5, dimnames = list(rnames, cnames))
c1 <- '95\\% confidence intervals for 5 binomial '
c2 <- 'experiments, each with $n_{S}$ successes in $n$ '
c3 <- 'trials.'
#latex(binomialCI, label = 'table: binomialCI', 
#   cdec = c(0, 0, 3, 3, 3), col.just = c('c', 'c|', 'c', 'c', 'c'), 
#   collabel.just = c('c', 'c|', 'c', 'c', 'c'), 
#   cgroup = c('', '', '', 'Confidence intervals'), 
#   n.cgroup = c(1, 1, 1, 2), rowlabel = '', 
#   caption = paste(c1, c2, c3), where = "!htbp", 
#   ctable = TRUE)
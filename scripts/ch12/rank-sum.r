rm(list = ls())
options(width = 60)
X.1 <-c (253, 218, 292, 280, 276, 275)
X.2 <- c(216, 291, 256, 270, 277, 285)
sample <- c(rep(1, 6), rep(2, 6))
w <- data.frame(c(X.1,X.2), sample)
names(w)[1] <- 'weight (g)'
cbind(w[1 : 6, ], w[7 : 12, ])

idx <- sort(w[, 1] , index.return = TRUE)
d <- rbind(weight = w[idx$ix, 1], sample = w[idx$ix, 2], 
  rank = 1:12)
dimnames(d)[[2]] <- rep('', 12) ; d
  
rank.sum <- c(sum(d[3, d[2, ] == 1]), 
  sum(d[3, d[2, ] == 2]))
rank.sum <- rbind( sample = c(1,2),
  'rank sum' = rank.sum)
dimnames(rank.sum)[[2]] <- c('','') ; rank.sum

wilcox.test(X.1, X.2)
wilcox.test(X.1, X.2, mu = 50, alternative = 'less')

# some equal ranks

options(width = 60)
X.1 <- c(3, 4, 5, 7, 3, 8)
X.2 <- c(10, 6, 9, 1, 2, 7)
wilcox.test(X.1, X.2)
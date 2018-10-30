rm(list = ls())

load('test.scores.rda')
(z <- test.scores.rda)
W.plus <- sum(z$signed.rank[z$signed.rank > 0])
W.minus<-  - sum(z$signed.rank[z$signed.r < 0])
c(W.plus, W.minus)

W <- min(W.plus, W.minus)
round(c('W' = W, 'p.value' = 2 * 
   psignrank(W, length(z[, 1]), length(z[, 1]))), 3)
wilcox.test(midterm, final, paired = TRUE)


par(mfrow = c(1, 2))
qqnorm(midterm, main = 'midterm') ; qqline(midterm)
qqnorm(final, main = 'final') ; qqline(final)

rm(list = ls())
load('capital.punishment.rda')
attach(capital.punishment)
color <- ifelse(Race == 'Black', 'Black', 'Other')
status <- ifelse(MaritalStatus == 'Married', 
   'Married', 'Single')
x <- data.frame(color, status)
head(x, 4)

set.seed(100)
idx <- sample(1 : length(color), 400)
s <- x[idx, ]

s <- table(s)
rbind(cbind(s, Total = c(40 + 136, 45 + 170)),
  Total = c(40 + 45, 136 + 179, 176 + 215))

n.1S <- 40 ; n.2S <- 45 ; n.1F <- 136 ; n.2F <- 179 ;
n.1 <- n.1S + n.1F ; n.2 <- n.2S + n.2F ; n <- n.1 + n.2
pi.1 <- n.1S / n.1 ; pi.2 <- n.2S / n.2
pi.hat <- n.1 / n * pi.1 + n.2 / n * pi.2

E <- c(pi.hat * n.1, pi.hat * n.2, 
  (1 - pi.hat)* n.1, (1 - pi.hat) * n.2)
O <- c(n.1S, n.2S, n.1F, n.2F)
(chisq.value <- sum((abs(O - E) - 0.5)^2 / E))
(p.value <- 1 - pchisq(chisq.value, 1))

chisq.test(s)
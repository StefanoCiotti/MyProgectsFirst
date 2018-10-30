rm(list = ls())
load('capital.punishment.rda')
attach(capital.punishment)
color <- ifelse(Race == 'Black', 'Black', 'Other')
status <- ifelse(MaritalStatus == 'Married', 
   'Married', 'Single')
x <- data.frame(color, status)
set.seed(100)
idx <- sample(1 : length(color), 400)
s <- x[idx, ]
chisq.test(table(s))
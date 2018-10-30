
rm(list = ls())
options(width = 60)
my_dir <- "I:/Corso_di_R/_printed/Statistics and Data with R/scripts/ch02"
setwd(my_dir)
(WD <- getwd())

load('capital.punishment.rda')
cp <- capital.punishment
unique(cp[, c('Method', 'Sex')])
(tbl <- table(cp[, c('Method', 'Sex')]))
library(epitools)
(e.tbl <- expand.table(tbl[3 : 5, ]))
tapply(e.tbl$Method, e.tbl$Method, length)

a <- LETTERS[1 : 10]
set.seed(7)
midterm <- round(rnorm(10, 75, 10))
final <- round(rnorm(10, 80, 5))
scores <- data.frame(a, midterm, final)
stack(scores)
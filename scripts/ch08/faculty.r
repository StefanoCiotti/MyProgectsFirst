rm(list = ls())
name <- c('Ira A', 'David A', 'Todd A', 'Robert B',
   'Yosef C', 'James C', 'Francesca A', 'David F', 
   'Rocky G', 'Peter J', 'Anne K', 'Kristen N', 'Ray N', 
   'James P', 'Peter S', 'George S', 'Ellen S', 'Bruce V')
height <- c(5 + 4 / 12, 6 + 11 / 12, 5 + 11 / 12, 
   5 + 11 / 12, 6, 5 + 10 / 12, 5 + 10 / 12, 5 + 11 / 12, 
   5 + 3 / 12, 5 + 10 / 12, 5 + 8 / 12, 5 + 7 / 12, 
   5 + 10 / 12, 5 + 9 / 12, 5 + 10.5 / 12, 5 + 10.5 / 12, 
   5 + 10 / 12, 6)
faculty <- data.frame(name, height)
save(faculty, file = 'faculty')
set.seed(1) ; mean(sample(faculty$height, 5))


report <- rbind(
  c(var(faculty$height), var(faculty$height[-2])),
  summary(faculty$height)[c(2, 5)],
  summary(faculty$height[-2])[c(2, 5)])
dimnames(report) <- list(
  c('sigma^2', 'IQR with David A.', 'IQR without'),
  c(' ', ' '))
round(report, 2)



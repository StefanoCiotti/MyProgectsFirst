op <- options(width = 60, stringsAsFactors = FALSE)
(begin.patients <- paste(LETTERS[1 : 6], '.', ' Smith',
  sep = ''))
set.seed(21)
(begin.weight <- round(rnorm(6, 250, 25)))
begin.experiment <- data.frame(name = begin.patients,
  weight = begin.weight)
begin.experiment

(middle.patients <- paste(LETTERS[7 : 9], '.', ' Smith',
  sep = ''))
(middle.weight <- round(rnorm(3, 200, 20)))
middle.experiment <- data.frame(name = middle.patients,
  weight = middle.weight)
middle.experiment

(end.patients <- c(sample(begin.patients, 3),
  sample(middle.patients, 2)))
(end.weight <- round(rnorm(5, 100, 5)))
end.experiment <- data.frame(name = end.patients,
  weight = end.weight)
end.experiment
  
(m <- is.element(begin.experiment$name, end.experiment$name))
(begin.end <- begin.experiment[m, ])
(p.names <- begin.experiment[m, 1])
(patients <- cbind(begin.experiment[m, ],
  end.experiment[is.element(end.experiment$name, p.names), ]))
(p.names <- stack(patients[, c(1, 3)]))
(weights <- stack(patients[, c(2, 4)])[, 1])
(experiment <- data.frame(p.names, weights))
levels(experiment$ind) <- c('begin', 'end')
names(experiment)[1 : 2] <- c('name', 'time')
experiment
tapply(experiment$weights, experiment$time, mean)


















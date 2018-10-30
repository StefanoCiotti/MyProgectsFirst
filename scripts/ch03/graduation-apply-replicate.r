rm(list = ls())
graduation <- read.table('graduation.txt',
  header = TRUE, sep = '\t')
names(graduation) <- c('region', 'state', '% 00-01',
  'n 00-01', '% 01-02', 'n 01-02',
  '% 02-03', 'n 02-03')
(head(graduation[, 1 : 6], 3))

region <- c(as.character(state.region[1 : 8]),
  as.character(state.region[4]),
  as.character(state.region[9 : 50]))
graduation$region <- as.factor(region)
(head(graduation[, 1 : 6], 3))

(round(apply(graduation[, 3 : 8], 2, mean), 1))

pct <- round(apply(graduation[, c(3, 5, 7)], 1, mean), 1)
n <- round(apply(graduation[, c(4, 6, 8)], 1, mean), 1)
by.state <- data.frame(graduation[, 'state'], pct, n)
names(by.state) <- c('state', '%', 'n')
head(by.state)

pct.region <- round(tapply(by.state[, 2],
  graduation$region, mean), 0)
n.region <- round(tapply(by.state[, 3],
  graduation$region, mean), 0)
rbind('%' = pct.region, n = n.region)

grad.split <- split(graduation[, 3 : 8],
  graduation$region)
names(grad.split)
round(sapply(grad.split, mean), 0)
round(apply(grad.split$West, 2, mean), 0)

pct.region <- round(sapply(by.state[, 2], mean, simplify = TRUE), 0)
n.region <- round(sapply(by.state[, 3], mean, simplify = TRUE), 0)
rbind('%' = pct.region, n = n.region)
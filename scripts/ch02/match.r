rm(list = ls())
op <- options(width = 60, stringsAsFactors = FALSE)

set.seed(11)
id <- 100 : 109
(NAME <- paste(LETTERS[1 : 10], '.', ' Smith', sep = ''))
(name.id <- data.frame(id, NAME))
(aggressive <- round(runif(20, 0, 10)))
(id.agressive <- data.frame(cbind(id,
  sample(aggressive, 4, replace = TRUE))))
match(id.agressive[, 1], name.id[, 1])













rm(list = ls())
options(stringsAsFactors = TRUE)
UN <- read.table('POP-R2002-DATA_Web.txt', header = TRUE,
  sep = '\t')
names(UN)[c(6, 7, 8, 9, 11)] <- c('% urban', '% growth',
  'birth rate', 'death rate', 'under 5 mortality')

par(mar = c(16, 4, 4, 2) + 0.1)
bp <- boxplot(UN[, 7] ~ UN[, 1], las = 2,
  main = '% growth rate by continent')
identify(UN[, 7] ~ UN[, 1], labels = UN$country)

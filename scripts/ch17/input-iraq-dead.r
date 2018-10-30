rm(list = ls())
library(CTFS)

iraq.dead <- read.table('iraq.dead.txt', 
  header = TRUE, sep = '\t')
iraq.dead$Date <- as.Date(iraq.dead$Date, '%m/%d/%Y')
# origina = 1/1/1960, NOT 1/1/1970
jd <- tojulian(iraq.dead$Date, dateform = "%Y-%m-%d")
iraq.dead$Julian <- jd

(names(iraq.dead))
head(iraq.dead)
save(iraq.dead,file='iraq.dead.rda')

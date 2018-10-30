# Fleiss et al. 2003, Table 11.1 : 
trisomy.table <- read.table('trisomy-and-maternal-age.txt', 
	header = TRUE, sep = '\t')
dimnames(trisomy.table)[[1]] <- trisomy.table$age
trisomy.table <- 
  trisomy.table[, 2 : length(trisomy.table[1, ])]
save(trisomy.table, file = 'trisomy.table.rda')
trisomy.table

# create 'data'
trisome <- vector(); age <- vector()
for(i in 1 : length(trisomy.table[, 1])){
	trisome <- c(trisome, rep(TRUE, trisomy.table[i, 2]), 
      rep(FALSE, trisomy.table[i, 3]))
	age <- c(age, rep(trisomy.table[i, 1], 
      trisomy.table[i, 4]))
}
trisomy <- data.frame(age, trisome)
save(trisomy, file = 'trisomy.rda')
head(trisomy, 4)
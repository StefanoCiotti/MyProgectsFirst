library(Hmisc) ; e <- read.csv('elections-2000.csv')
m <- as.matrix(e[1 : 5, -1])
dimnames(m)[[1]] <- sub.e[, 1]

cap1 <- 'Number of votes by county and candidate.'
cap2 <- 'US 2000 presidential elections, Florida counts.'

latex(m, file = 'election.tex', 
   label = 'table: election-2000', cdec = rep(0, 4),
   col.just = rep('r ', 4), collabel.just = rep('c ', 4),
   rowlabel = 'County', rowlabel.just = 'l|', 
   caption = paste(cap1, cap2), 
   where = '!h t b p', ctable = TRUE)

cap1 <- 'Number of votes by candidate and county.'
cap2 <- 'US 2000 presidential elections, Florida counts.'

latex(t(m), file = 't-election.tex',
   label = 'table: t-election-2000', cdec = rep(0, 5),
   col.just = rep('r ', 5), collabel.just = rep('c ', 5),
   rowlabel = 'Candidate', rowlabel.just = 'l|', 
   caption = paste(cap1, cap2), 
   where = '!h t b p', ctable = TRUE)
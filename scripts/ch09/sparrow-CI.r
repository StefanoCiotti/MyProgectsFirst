library(Hmisc)

rnames <- c(
   '\\emph{A. b. Cinera} (male)', 
   '\\emph{A. b. Canescens} (male)', 
   '\\emph{A. b. Nevadensis} (male)', 
   '\\emph{A. b. Cinera} (female)', 
   '\\emph{A. b. Canescens} (female)', 
   '\\emph{A. b. Nevadensis} (female)'
   )
cnames <- c('Chord (mm)', 'sd', '$n$')
dat <- c(65.4, 3.1, 13, 70.9, 2.88, 45, 78.7, 2.79, 
   38, 63.0, 2.77, 12, 67.2, 2.77, 42, 73.4, 2.3, 30)
sparrowCI <- matrix(dat, nrow = length(rnames), 
   ncol = length(cnames), dimnames = list(rnames, cnames), 
   byrow = TRUE)

latex(sparrowCI, label = 'table: sparrow-CI', 
   cdec = c(2, 2, 0), col.just = rep('c', length(cnames)), 
   rowlabel = 'Subspecies', 
   caption = 'Chord lengths of 3 sage sparrow subspecies.', 
   where = "!htbp", ctable = TRUE)

alpha <- 0.05; df <- 12
b <- qt(1 - alpha / 2, 12) * 
   sparrowCI[1, 2] / sqrt(sparrowCI[1, 3])
round((ci <- c(sparrowCI[1, 1] - b, sparrowCI[1, 1] + b)), 2)
nucleotide <- c('U', 'C', 'A', 'G')
library(gtools)
RNA.codons <- combinations(4, 3, v = nucleotide,
  repeats.allowed = TRUE)
RNA.codons <- data.frame(RNA.codons[1 : 10, ], '   ',
  RNA.codons[11 : 20, ])
nqd(as.matrix(RNA.codons))
nucleotide <- c('U', 'C', 'A', 'G')
library(gtools)
RNA.codons <- permutations(4, 3, v = nucleotide,
  repeats.allowed =  TRUE)
RNA.table <- data.frame(RNA.codons[ 1 : 16, ], '   ')
for(i in 2 : 4){
  RNA.table <- data.frame(
    RNA.table, RNA.codons[ (16 * (i - 1) + 1) : (16 * i),
    ], '   ')
}
nqd(as.matrix(RNA.table))
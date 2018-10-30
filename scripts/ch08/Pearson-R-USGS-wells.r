rm(list = ls())
load('wells.info.rda')
wells.info
load('wells.nucleotides.rda')
s <- split(wells.nucleotides, wells.nucleotides$nucleotide)
nuc <- data.frame(s[[1]]$reading, s[[2]]$reading,
  s[[3]]$reading)
names(nuc) <- names(s)
openg(4.5, 4.5)
pairs(nuc)
saveg('Pearson-R-USGS-wells', 4.5, 4.5)
round(cor(nuc, use = 'pairwise.complete.obs'), 2)
round(cor(log(nuc), use = 'pairwise.complete.obs'), 2)







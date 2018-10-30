nucleotides <- read.table('nucleotides-usgs.txt',
	sep = '\t', header = TRUE)
Ra224 <-nucleotides[nucleotides[, 5] == 'Ra224', 2]
Ra226 <-nucleotides[nucleotides[, 5] == 'Ra226', 2]
Ra228 <-nucleotides[nucleotides[, 5] == 'Ra228', 2]
nuc <-data.frame(Ra224, Ra226, Ra228)
library(MASS)
pairs(nuc)
windows()
pairs(log(nuc))
source('bootstrap.R')
nuc.mean.boot <- bootstrap(nuc,mean,1000,list(na.rm=TRUE))
print(summary(nuc.mean.boot))
nuc.var.boot <- bootstrap(nuc,var,1000,list(na.rm=TRUE))
print(summary(nuc.var.boot))

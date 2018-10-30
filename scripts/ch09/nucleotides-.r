nucleotides<-read.table('nucleotides-usgs.txt',
   header=TRUE,sep='\t')
nucleotides[!is.na(nucleotides[,3]) & nucleotides[,5]=='Ra228',3] <- 
   nucleotides[!is.na(nucleotides[,3]) & nucleotides[,5]=='Ra228',3]/2
names(nucleotides)[3]<-'sd'
save(nucleotides,file='nucleotides.rda')

wells<-read.table('nucleotides-usgs.txt',
   header=TRUE,sep='\t')
save(wells,file='wells.rda')

name<- c("USGS.SN", "result", "sd", "mdc", "nucleotide")
explanation<-c('USGS serial numer', 
   'reading in pCi/L (pico-Curie per liter', 
   'standard deviation', 
   'minimum detectable concentration (1 SD)',
   'of radium (Ra-224, Ra226 or Ra228)')
nucleotides.info<-data.frame(name,explanation)
save(nucleotides.info,file='nucleotides.info.rda')

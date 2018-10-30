nucleotides<-read.table('nucleotides-usgs.txt',
   header=TRUE,sep='\t')
nucleotides[!is.na(nucleotides[,3]) & nucleotides[,5]=='Ra228',3] <- 
   nucleotides[!is.na(nucleotides[,3]) & nucleotides[,5]=='Ra228',3]/2
names(nucleotides)[3]<-'sd'
save(nucleotides,file='nucleotides.rda')

wells<-read.table('nucleotides-wells.txt',
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


w<-4.5 ; h=2.4
openg(width=w,height=h)
par(mfrow=c(1,2))
qqnorm(nucleotides$result,main='result')
qqline(nucleotides$result)
qqnorm(log(nucleotides$result),main='log(result)')
qqline(log(nucleotides$result))
saveg('exercise-nucleotides-qq',width=w,height=h)



result<-nucleotides$result[!is.na(nucleotides$result)]
xbar<-mean(log(result[result>0]))
xbar
exp(xbar)

length(result[result>5])/(length(result))


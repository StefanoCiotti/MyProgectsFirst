load('nucleotides.rda')

mean.nucleotides<-tapply(nucleotides[,2],
   nucleotides[,5],mean,na.rm=TRUE)
sd.nucleotides<-tapply(nucleotides[,2],nucleotides[,5],
   sd,na.rm=TRUE)
n.nucleotides<-rep(90,3)

nucleotides.summary<-data.frame(mean=mean.nucleotides,
   sd=sd.nucleotides,n=n.nucleotides)

term<-qnorm(0.95)*nucleotides.summary$sd/sqrt(nucleotides.summary$n)
low<-nucleotides.summary$mean-term
high<-nucleotides.summary$mean+term

nucleotides.summary<-data.frame(nucleotides.summary,low,high)

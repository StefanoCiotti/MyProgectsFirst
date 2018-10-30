load('nucleotides.rda')

Ra224<-nucleotides[nucleotides[,5]=='Ra224',2]
Ra226<-nucleotides[nucleotides[,5]=='Ra226',2]
Ra228<-nucleotides[nucleotides[,5]=='Ra228',2]
nuc<-data.frame(Ra224,Ra226,Ra228)

w<-4.5; h<-4.5
openg(height=h,width=w)
pairs(nuc)
saveg('exercise-nucleotide-pairs',height=h,width=w)

openg(height=h,width=w)
pairs(log(nuc))
saveg('exercise-nucleotide-pairs-log',height=h,width=w)


nuc.mean.boot<-boot(nuc,mean,arguments=list(na.rm=TRUE))
openg(height=h,width=w)
plot(nuc.mean.boot,main='')
saveg('exercise-nucleotide-pairs-mean-bootstrap',height=h,width=w)

nuc.var.boot<-boot(nuc,var,arguments=list(na.rm=TRUE))
openg(height=h,width=w)
plot(nuc.var.boot,main='')
saveg('exercise-nucleotide-pairs-var-bootstrap',height=h,width=w)

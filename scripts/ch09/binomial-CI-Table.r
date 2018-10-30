library(Hmisc)

nc<-30
cnames<-c('$n$','$n_{S}$','$p$','Low','High','Low','High','Low','High')
rnames=' '
binomialCITable <-matrix(1:9,nrow=1,ncol=9,dimnames=list(rnames,cnames))

for(n in 1:nc){
   for(nS in 0:i){
      if(nS<=n){
         p<-nS/n; np<-n*p; nq<-n*(1-p)
         if ((np<=5)|(nq<=5))
            binomialCITable<-rbind(binomialCITable,
               c(n,nS,p,as.vector(binconf(nS,n,.1))[2:3],
                  as.vector(binconf(nS,n,.05))[2:3],
                  as.vector(binconf(nS,n,.01))[2:3]))
         
      }
   }
}
binomialCITable<-binomialCITable[-1,]
names(binomialCITable)<-cnames
dimnames(binomialCITable)[[1]]<-rep(' ',length(binomialCITable[,1]))

w  <-  latex(binomialCITable,
   file='binomialCITable.tex',
   label='table: binomialCITable',
   cdec=c(0,0,rep(3,7)), 
   col.just=c('c','c','c|',rep('c',6)),
   collabel.just=c('c','c','c|',rep('c',6)),
   cgroup=c('','','','90\\%','95\\%','99\\%'),
   n.cgroup=c(1,1,1,2,2,2),
   rowlabel=' ',
   rowname=' ',
   caption='Confidence intervals for the binomial distribution for confidence levels = 0.9, 0.05 and 0.01. $n$ $=$ number of trials, $n_{S}$ $=$ number of successes, $p$ $=$ point estimate of $\\pi$.',
   na.blank=TRUE,
   longtable=TRUE,
   lines.page=38,
   where="!p",
   ctable=FALSE)


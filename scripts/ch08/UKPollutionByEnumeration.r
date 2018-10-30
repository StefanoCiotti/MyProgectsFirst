vessels<-c('tanker','fishing','support','coastal.tanker','cargo',
   'pleasure.craft','wreck','other')
pbe<-read.table('UK.pollution.by.enumeratin.txt',sep='\t')
names(pbe)<-vessels
pbe
pairs(pbe)
r<-matrix(nrow=8,ncol=8)
for(i in 1:8){
   for (j in 1:8){
      temp<-cor.test(pbe[,i],pbe[,j],method='spearman')
     r[i,j]=temp[[4]][[1]]
   }
}
dimnames(r)[[2]]<-vessels
dimnames(r)[[1]]<-vessels
r




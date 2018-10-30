uk.metals<-read.csv('UK.txt',sep='\t')
uk.north.sea<-t(uk.metals[,3:16])
dimnames(uk.north.sea)[[2]]<-c('Arsenic',
   'Cadmium',
   'Chromium',
   'Copper',
   'Nickel',
   'Lead',
   'Titanium',
   'Zinc'
)
dimnames(uk.north.sea)[[1]]<-c(1987:2000)
uk.north.sea
pairs(uk.north.sea)
r<-matrix(nrow=8,ncol=8)
for(i in 1:8){
   for (j in 1:8){
      temp<-cor.test(uk.north.sea[,i],uk.north.sea[,j])
     r[i,j]=temp[[4]][[1]]
   }
}
n<-c('Arsenic',
   'Cadmium',
   'Chromium',
   'Copper',
   'Nickel',
   'Lead',
   'Titanium',
   'Zinc'
)
dimnames(r)[[2]]<-n
dimnames(r)[[1]]<-n
r



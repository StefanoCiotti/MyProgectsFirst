vessels<-c('tanker','fishing','support','coastal.tanker','cargo',
+   'pleasure.craft','wreck','other')
pbe<-read.table('UK.pollution.by.enumeratin.txt',sep='\t')
names(pbe)<-vessels

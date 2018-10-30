library(gtools)
perm<-data.frame(permutations(4,2))
average<-vector();
for(i in 1:length(perm[,1])) average[i]<-mean(t(perm[i,]))
perm<-data.frame(perm,average)
comb<-combinations(4,2)
average<-vector();
for(i in 1:length(comb[,1])) average[i]<-mean(t(comb[i,]))
comb<-data.frame(comb,average)

table(perm$average)/12
table(comb$average)/6
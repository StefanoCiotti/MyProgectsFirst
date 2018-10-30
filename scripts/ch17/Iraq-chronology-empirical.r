#rm(list = ls())
library(CTFS)
load('iraq.dead.rda')
load('iraq.daily.deaths.rda')

# levels(iraq.dead$general.location)
idx <- c(7, 42, 60, 62, 66, 69, 89, 136, 161, 177, 195)
n <- length(idx)
count.dead = list()
j <- 1
for(i in idx){
  f <- as.character(levels(iraq.dead$general.location)[idx[j]])
  tmp <- iraq.dead$Julian[iraq.dead$general.location == f]
  count.dead[[j]] <- as.data.frame(table(tmp))
  names(count.dead[[j]]) <- c('Julian', 'Dead')
  j <- j + 1
}
names(count.dead) <- 
  as.character(levels(iraq.dead$general.location)[idx])

first.julian <- iraq.dead$Julian[1]
last.julian <- iraq.dead$Julian[length(iraq.dead$Julian)]
n <-  last.julian - first.julian + 1
A <- N <- vector()
for(i in 1:length(count.dead)){
  A[i] <- names(count.dead)[i]
  N[i] <- length(count.dead[[i]]$Julian)
}
cardinality <- data.frame(A, N)
# create P(A_i)
PAi <- data.frame(cardinality, 'P(A_i)' = 
  cardinality$N / n)
# create P(A_i \cap A_j)
PAi.cap.Aj <- matrix(ncol=11,nrow=11)
for (i in 1:length(count.dead))
  for(j in 1:length(count.dead))
    PAi.cap.Aj[i,j] <- length(intersect(count.dead[[i]][,1],
      count.dead[[j]][,1]))/n
    
# the empirical conditional matrix
PAi.given.Aj <- matrix(ncol=11,nrow=11)
for(j in 1:length(count.dead))
  for(i in 1:length(count.dead))
    PAi.given.Aj[i,j] <- PAi.cap.Aj[i,j]/PAi[j,3]
rownames(PAi.given.Aj) <- as.character(PAi[,1])
colnames(PAi.given.Aj) <- as.character(PAi[,1])


# rows
row.sum <- vector()
for(i in 1:11) row.sum[i] <- sum(PAi.given.Aj[i,])
PAi.given.Aj <- cbind(PAi.given.Aj, row.sum)
# columns
column.sum <- vector()
for(j in 1:12) column.sum[j] <- sum(PAi.given.Aj[,j])
PAi.given.Aj <- rbind(PAi.given.Aj, column.sum)
print(round(PAi.given.Aj,3))  
save(PAi.given.Aj, file = 'PAi.given.Aj.rda')

idx <- sort(PAi.given.Aj[,12], decreasing = TRUE, 
  index.return = TRUE)
rs <- data.frame(location = 
  as.character(row.names(PAi.given.Aj)[idx$ix]), 
  p = PAi.given.Aj[idx$ix,12],
  stringsAsFactors = FALSE)
par(mar = c(9, 4, 1, 1) + 0.1)
barplot(rs[-1, 2] ,names.arg = rs[-1,1], las = 2,
  ylim = c(0, 5), ylab = 
    expression(italic( 
    paste( P,'(',A[i] ,'|',A.,')' )  )))

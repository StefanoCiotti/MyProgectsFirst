rm(list = ls())
library(CTFS)
load('iraq.dead.hostile.rda')
load('iraq.daily.deaths.rda')

TESTING <- FALSE
n.simulations <- 1000

# levels(iraq.dead.hostile$general.location)
idx.locations <- c(7, 42, 60, 62, 66, 69, 89, 
  136, 161, 177, 195)
n.locations <- length(idx.locations)
count.dead = list()
j <- 1
for(i in idx.locations){
  f <- as.character(levels(
    iraq.dead.hostile$general.location)[idx.locations[j]])
  tmp <- iraq.dead.hostile$Julian[iraq.dead.hostile$general.location == f]
  count.dead[[j]] <- as.data.frame(table(tmp))
  names(count.dead[[j]]) <- c('Julian', 'Dead')
  j <- j + 1
}
names(count.dead) <- 
  as.character(
    levels(iraq.dead.hostile$general.location)[idx.locations])

first.julian <- iraq.dead.hostile$Julian[1]
last.julian <- iraq.dead.hostile$Julian[length(iraq.dead.hostile$Julian)]
n.julian <-  last.julian - first.julian + 1

A <- N <- vector()
for(i in 1:n.locations){
  A[i] <- names(count.dead)[i]
  N[i] <- length(count.dead[[i]]$Julian)
}
# total number of Juians per location
cardinality <- data.frame(A, N)

# list to collect the prb matrices
simulation <- list()

## simulation

  # create P(A_i) : only once
  PAi <- data.frame(cardinality, 'P(A_i)' = 
    cardinality$N / n.julian)
    
  # create P(A_i \cap A_j) for nsim; A_i varies, A_j fixed
  for(s in 1:n.simulations){
    PAi.cap.Aj <- matrix(ncol=n.locations, nrow=n.locations)
    for (i in 1:n.locations){
          for(j in 1:n.locations)
        # generate random julians
        if (TESTING){
          PAi.cap.Aj[i,j] <- length(intersect(count.dead[[i]][,1],
            count.dead[[j]][,1]))/n.julian
        } else { 
          set.seed(s)        
          PAi.cap.Aj[i,j] <- length(intersect(
            sample(first.julian:last.julian,cardinality$N[i]),
            count.dead[[j]][,1]))/n.julian
        }
      }
    # the simulated conditional matrix
    PAi.given.Aj <- matrix(ncol=11,nrow=11)
    for(j in 1:n.locations)
      for(i in 1:n.locations)
        PAi.given.Aj[i,j] <- PAi.cap.Aj[i,j]/PAi[j,3]
    rownames(PAi.given.Aj) <- as.character(PAi[,1])
    colnames(PAi.given.Aj) <- as.character(PAi[,1])
  
    # row sums
    row.sum <- vector()
    for(i in 1:n.locations) row.sum[i] <- sum(PAi.given.Aj[i,])
    PAi.given.Aj <- cbind(PAi.given.Aj, row.sum)
    # column sums
    column.sum <- vector()
    for(j in 1:(n.locations + 1)) column.sum[j] <- sum(PAi.given.Aj[,j])
    PAi.given.Aj <- rbind(PAi.given.Aj, column.sum)
    simulation[[s]] <- PAi.given.Aj
  }
save(simulation, file = 'simulation.rda')
if (TESTING) {
  s <- 1
  print(round(simulation[[s]],3))  
  idx <- sort(simulation[[s]][,n.locations + 1], 
    decreasing = TRUE, index.return = TRUE)
  rs <- data.frame(location = 
    as.character(row.names(simulation[[s]])[idx$ix]), 
    p = simulation[[s]][idx$ix,n.locations + 1],
    stringsAsFactors = FALSE)
  par(mar = c(9, 4, 1, 1) + 0.1)
  barplot(rs[-1, 2] ,names.arg = rs[-1,1], las = 2,
    ylim = c(0, 5), ylab = 
      expression(italic( 
      paste( P,'(',A[i] ,'|',A.,')' )  )))
}
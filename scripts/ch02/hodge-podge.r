ch.v <- letters[1 : 5]                  #character vector
int.v <- as.integer(1 : 7)               # integer vector
m <- matrix(runif(10), ncol = 5, nrow = 2)       # matrix
(hodge.podge <- list(integers=int.v,               # list
   letter = ch.v, floats = m))

rbind(hodge.podge$letter,  hodge.podge[[2]])   # row bind

cbind(hodge.podge$letter[2 : 3],            # column bind
  hodge.podge[[2]][2 : 3]) 

length(hodge.podge)              # no. of list components
length(hodge.podge$floats)    # no. of elements in floats
length(hodge.podge$floats[,1])    # no. of rows in floats
length(hodge.podge$floats[1,]) # no. of columns in floats
length(hodge.podge[[3]][1,])   # no. of columns in floats
rnames <- c(
   '\\emph{A. b. Cinera} (male)', 
   '\\emph{A. b. Canescens} (male)', 
   '\\emph{A. b. Nevadensis} (male)', 
   '\\emph{A. b. Cinera} (female)', 
   '\\emph{A. b. Canescens} (female)', 
   '\\emph{A. b. Nevadensis} (female)'
   )
cnames <- c('Chord (mm)', 'SD', '$n$')
dat <- c(65.4, 3.1, 13, 70.9, 2.88, 45, 78.7, 2.79, 
   38, 63.0, 2.77, 12, 67.2, 2.77, 42, 73.4, 2.3, 30)
sparrowCI <- matrix(dat, nrow = length(rnames), 
   ncol = length(cnames), dimnames = list(rnames, cnames), 
   byrow = TRUE)

normal.test<-function(x,y,sig=.05){
   d<-x[1]-y[1]
   se<-sqrt(x[2]^2/x[3]+y[2]/y[3])
   Z<-d/se; z<-qnorm(1-sig/2)
   c('H0:'=Z>=z)
}

s<-sparrowCI
normal.test(s[1,],s[2,])
normal.test(s[1,],s[3,])
normal.test(s[2,],s[3,])
normal.test(s[4,],s[5,])
normal.test(s[4,],s[6,])
normal.test(s[5,],s[6,])

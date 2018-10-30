load( 'college.crime.rda' )
library( ctest )
cc <- college.crime    # for easy reference
re <- c( 2520, 56348 ) # range of enrollment
rf <- c ( 1, 10378 )   # range of full-time faculty
e <- 8; f <- 18        # column index of enrollment and faculty
pu <- 1; pr <- 2       # public or private school
pp <- 10               # column index of instition (pu,pr)

condition <- cc[,e] >= re[1] & cc[,e] <= re[2] &
   cc[,f] >= rf[1] & cc[,f] <= rf[2] #& cc[, pp] == pu

r <- cc[ condition , e ] / cc[ condition, f ]
r <- log( r )

trim <- c( 0, .1, .2 )
means <- c( mean( r, trim = trim[ 1 ] ), 
   mean( r,trim = trim[ 2 ] ),
   mean( r,trim = trim[ 3 ] ) )
print(means)

width <- 3; height <- 3; pointsize <- 8
windows( width = width, height = height, pointsize = pointsize )
qqnorm( r, main = 'log(enrollment / faculty)')
qqline( r )

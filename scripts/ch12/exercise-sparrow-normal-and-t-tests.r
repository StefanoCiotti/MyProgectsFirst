rnames <- c('A. b. Cinera (male)', 
   'A. b. Canescens (male)', 
   'A. b. Nevadensis (male)', 
   'A. b. Cinera (female)', 
   'A. b. Canescens (female)', 
   'A. b. Nevadensis (female)'
   )
cnames <- c('Chord (mm)', 'SD', 'n')
d <- c(65.4, 3.1, 13, 70.9, 2.88, 45, 78.7, 2.79, 
   38, 63.0, 2.77, 12, 67.2, 2.77, 42, 73.4, 2.3, 30)
sparrow <- matrix(d, nrow = length(rnames), 
   ncol = length(cnames), dimnames = list(rnames, cnames), 
   byrow = TRUE)

test <- function(X, z.t = 'Z'){
   X.bar <- X[2, 1] - X[1, 1]
   SE <- sqrt(sum(X[, 2]^2 / X[, 3]))
	if(z.t == 'Z') {
   stat <- X.bar / SE ; x.975 <- qnorm(.975)
  }
  else {
   stat <- X.bar / SE ; x.975 <- qt(.975, sum(X[, 3]-2))
  }
  if (stat > x.975) 'reject' else 'do not reject'
}

n <- 6
conclusion <- data.frame('subspecies'= NA, 'vs subspecies' = NA, 
	test = NA, 'HO' = NA)
s <- sparrow ; k <- 1
for(i in 1 : (n-1)){
	for(j in (i+1) : n){
		if (s[i, 3] >= 30 & s[j, 3] >= 30) z.t <- 'z' else z.t <- 't'
		conclusion[k, ] <- c(dimnames(s)[[1]][i], dimnames(s)[[1]][j], 
			z.t, test(rbind(s[i, ], s[j, ]), z.t))
		k <- k + 1
	}
}
print(conclusion)
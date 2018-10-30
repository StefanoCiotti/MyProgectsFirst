zp <- function(alpha = 0.05 , n , nS , pi0 , 
	HA = c('greater' , 'smaller' , 'neq')){
	
	p <- nS / n ; se <- sqrt(p * (1 - p) / n)
	correction <- 1/(2 * n)
	ifelse(correction < abs(p - PI) , correction , 0)
	Z <- (p - PI - correction) / se 
	critical.z <- qnorm(1 - alpha)
	HO <- ifelse(Z > critical.z , 'reject' , 
		'do not reject')
	if (HA == 'smaller') {
		critical.z <- qnorm(alpha)
		HO <- ifelse(Z < critical.z , 'reject' , 
			'do not reject')
	} 
	if (HA == 'neq') {
		critical.z <- qnorm(1 - alpha / 2)
		HO <- ifelse(abs(Z) > critical.z , 'reject' , 
			'do not reject')
	}
	results <- list(Z , critical.z , alpha , HO)
	names(results) <- c('Z' , 'critical.z' , 'alpha' , 'HO')
	class(results) <- 'table'
	print(results)
}
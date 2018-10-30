t.test <- function(x, mu = 0, alpha = 0.05 , 
	HA = c('greater' , 'smaller' , 'neq')){
	
  if(!missing(mu) && (length(mu) != 1 || is.na(mu)))
      stop("'mu' must be a single number")
  if(missing(HA)) stop("For HA, specify one of 'greater', 
  	'smaller' or 'neq'")
	X.bar <- mean(x, na.rm = TRUE)
	se <- sd(x) / sqrt(length(n))
	T <- (X.bar - mu) / se 
	df <- length(x) - 1
	critical.t <- qt(1 - alpha, df)
	HO <- ifelse(T > critical.t , 'reject' , 
		'do not reject')
	if (HA == 'smaller') {
		critical.t <- qt(alpha, df)
		HO <- ifelse(T < critical.t , 'reject' , 
			'do not reject')
	} 
	if (HA == 'neq') {
		critical.t <- qt(1 - alpha / 2, df)
		HO <- ifelse(abs(T) > critical.t , 'reject' , 
			'do not reject')
	}
	results <- list(T , critical.t , alpha , HO)
	names(results) <- c('T' , 'critical.t' , 'alpha' , 'HO')
	class(results) <- 'table'
	print(results)
}
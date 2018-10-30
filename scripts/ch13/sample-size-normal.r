sample.size.normal <- function(mu = 0, S.1, S.2, 
   k = 1, alpha = 0.05, power = 0.8, alt = "two.sided")
{

   # functions --------------------------------------------
   
	normal.sample.size <- function(z.alpha, z.power, 
	   S.1, S.2, k, mu, two.samples)
	{
		n.1 <- ((S.1 * (z.alpha + z.power))/mu)^2
		if(two.samples) {
			n.1 <- n.1 * (1 + S.2^2/(k * S.1^2))
		}
		return(ceiling(n.1))
	}

   # main function ----------------------------------------

	if(!missing(alt)) {
		alt.expanded <- char.expand(alt, 
		c("two.sided", "greater", "less"), 
		stop(paste("argument 'alt' must match one of", 
		"'greater', 'less', 'two.sided'.")))
	}
	else {
		alt.expanded <- alt
	}

	if(missing(mu) || is.null(mu)) {
		stop("a detectable difference 'mu' must be specified")
	}
	
	if((missing(S.1) || is.null(S.1)) && 
	   (missing(S.2) || is.null(S.2))) {
		stop(paste("at least one sample standard ",
		"deviation 'S.1' must be specified"))
	}
	
	if(missing(S.2) || is.null(S.2)){
	   two.samples <- FALSE
	   S.2 <- NULL
	}
	else{
	   two.samples <- TRUE
	}
	
	size <- expand.grid(mu, power)
	size <- data.frame(size, 
	   vector(length = length(size[, 1])))
	names(size) <- c('mu', 'power', 'n.1')

	results <- list(mu = mu, S.1 = S.1,
	   S.2 = S.2, alpha = alpha, alternative = alt,
	   size = size)
	
	if(alt.expanded == "two.sided") {
		z.alpha <- qnorm(1 - alpha/2)
	}
	else {
		z.alpha <- qnorm(1 - alpha)
	}
	
	l <- 1
	for(i in 1 : length(mu)){
	   for(j in 1 : length(power)){
	      z.power <- qnorm(results$size$power[l])
	      results$size$n.1[l] <- normal.sample.size(z.alpha,
	         z.power, S.1, S.2, k, results$size$mu[l], 
	         two.samples)
	      l <- l + 1 
	   }
	}
	
	if(two.samples){
	   results$size <- data.frame(results$size, n.2 = results$size$n.1 * k)
	}
	
	return(results)
}

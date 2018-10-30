power.normal <- function(mu.A, mu.0 = 0, 
   n.1, n.2, S.1, S.2,   
   alpha = 0.05, alt = "two.sided")
{

   # functions --------------------------------------------   

	normal.power <- function(z.alpha, mu.A, mu.0, n.1, S.1, 
	   n.2, S.2, two.samples, alt)
	{	      
		if(!two.samples) {
			SE <- S.1 / sqrt(n.1)
		}
		else {
		   k <- n.2 / n.1
			SE <- sqrt((k * S.1^2 + S.2^2) / n.2)
		}
		delta <- mu.A - mu.0
		power <- switch(alt,
			greater = ifelse(delta == 0, 1 - pnorm(z.alpha), 
			   pnorm(delta / SE - z.alpha)),
			less = ifelse(delta == 0, 1 - pnorm(z.alpha), 
			   pnorm( - delta / SE - z.alpha)),
			two.sided = ifelse(delta == 0, 1 - pnorm(z.alpha), 
			   pnorm(delta  / SE - z.alpha)) + 
			   ifelse(delta == 0, 1 - pnorm(z.alpha), 
			   pnorm( - delta  / SE - z.alpha)))
		return(power)
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

	if((missing(n.1) || is.null(n.1)) && 
	   (missing(n.2) || is.null(n.2))) {
		stop(paste("at least one smaple size 'n.1'",
		" must be specified"))
	}
	
	if(missing(mu.A) || is.null(mu.A)) {
		stop("the alternative 'mu.A' must be specified")
	}
	
	if(missing(n.2) || is.null(n.2)){
	   two.samples <- FALSE
	   n.2 <- S.2 <- NULL
	}
	else{
	   two.samples <- TRUE
	}

	pwr <- data.frame(mu.A, power = rep(NA, length(mu.A)))
	results <- list(mu.0 = mu.0, n.1 = n.1, S.1 = S.1,
	   n.2 = n.2, S.2 = S.2, alpha = alpha, alternative = alt,
	   pwr = pwr)

	if(alt.expanded == "two.sided") {
		z.alpha <- qnorm(1 - alpha/2)
	}
	else {
		z.alpha <- qnorm(1 - alpha)
	}
	

  results$pwr$power <- normal.power(z.alpha = z.alpha,
    mu.A = mu.A, mu.0 = mu.0, n.1 = n.1, S.1 = S.1,
    n.2 = n.2, S.2 = S.2, two.samples = two.samples,
    alt = alt)

	return(results)
}
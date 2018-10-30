normal.sample.size <-
function(mean2, mean.alt, mean = 0, sd1 = 1, sd2 = sd1, power = 0.8, 
   alpha = 0.05, n1 = NULL, n2 = NULL, prop.n2 = 1, 
   one.sample = missing(sd2), alternative = "two.sided", 
   expand.args = TRUE, exact.n = FALSE, recompute.power = FALSE)
{
	#=====================
	# 
	# formulas
	#     
	#=====================
	#
	# The formulas used are from 
	#
	#   Biostatistics by Lloyd D. Fisher and Gerald Van Belle
	#   John Wiley & Sons, 1993
	#
	# Z_a = Z(1 - alpha/2) for two-tailed test, or Z(1 - alpha) for one tail.
	# Z_p = Z(1 - beta) 
	#
	# One-sample:
	#
	#    delta = abs(mean.null - mean.alt)
	#        n = {sigma * (Z_a + Z_b) / delta}^2
	#   
	# Two-sample:
	#
	#    delta = abs(mean2 - mean1)
	#       n1 = {1 + sigma2^2 / (k*sigma1^2) } * {sigma1 * (Z_a + Z_b) / delta}^2 
	#       n2 = k*n1
	#
	#=====================
	# 
	# functions
	#     
	#=====================
	#
	#--------------------------------------------
	#
	# Sample Size function
	#
	#--------------------------------------------
	compute.sample.size <- function(Zalpha, Zpower, sd1, sd2, prop.n2, delta, one.sample, exact.n, ...)
	{
		n <- ((sd1 * (Zalpha + Zpower))/delta)^2
		if(!one.sample) {
			n1 <- n * (1 + sd2^2/(prop.n2 * sd1^2))
			if(!exact.n) {
				n1 <- ceiling(n1)
				n2 <- ceiling(prop.n2 * n1)
			}
			else {
				n2 <- prop.n2 * n1
			}
			n <- list(n1, n2)
		}
		else if(!exact.n) {
			n <- ceiling(n)
		}
		return(n)
	}
	#--------------------------------------------
	#
	# Power function
	#
	#--------------------------------------------
	compute.power <- function(Zalpha, sd1, sd2, n1, n2, prop.n2, delta, one.sample, alternative, ...)
	{
		if(one.sample) {
			sigma.inv <- sqrt(n1)/sd1
		}
		else {
			sigma.inv <- sqrt(n2/(prop.n2 * sd1^2 + sd2^2))
		}
		###
		### if delta is 0, power should be alpha, but there is a 
		### chance that n1 is Inf, which would produce incorrect
		### results - so treat delta = 0 as special case
		###
		power <- switch(alternative,
			greater = ifelse(delta == 0, 1 - pnorm(Zalpha), pnorm(delta * sigma.inv - Zalpha)),
			less = ifelse(delta == 0, 1 - pnorm(Zalpha), pnorm( - delta * sigma.inv - Zalpha)),
			two.sided = ifelse(delta == 0, 1 - pnorm(Zalpha), pnorm(delta * sigma.inv - Zalpha)) + ifelse(delta == 0, 1 - pnorm(Zalpha), pnorm( - delta * sigma.inv - Zalpha)))
		return(power)
	}
	#--------------------------------------------
	#
	# Delta function
	#
	#--------------------------------------------
	compute.delta <- function(Zalpha, Zpower, sd1, sd2, n1, n2, prop.n2, one.sample, alternative, ...)
	{
		if(one.sample) {
			sigma <- sd1/sqrt(n1)
		}
		else {
			sigma <- sqrt((prop.n2 * sd1^2 + sd2^2)/n2)
		}
		delta <- (Zalpha + Zpower) * sigma
		if(alternative == "less") {
			delta <- ( - delta)
		}
		return(delta)
	}
	#-----------------------------------
	#
	# main section
	#
	#-----------------------------------

	#
	#
	# DEBUG: stop here to make sure arglist is correct
	#
	#arglist <- c(mean2 = mean2, mean.alt = mean.alt, mean = mean, sd1 = sd1, sd2 = sd2, power = power, 
   #   alpha = alpha, n1 = n1, n2 = n2, prop.n2 = prop.n2, 
   #   one.sample = one.sample, alternative = alternative, 
   #   expand.args = expand.args, exact.n = exact.n, recompute.power = recompute.power)
	# return(arglist)

	if(!missing(alternative)) {
		alt.expanded <- char.expand(alternative, c("two.sided", "greater", "less"), stop("argument 'alternative' must match one of 'greater', 'less', 'two.sided'."))
	}
	else {
		alt.expanded <- alternative
	}
	###
	### Determine what to compute and set that arg to NA for passing to 
	### build.power.table
	###
	if(is.null(n1) && is.null(n2)) {
		compute.what <- "sample.size"
		compute.function <- "compute.sample.size"
		n1 <- n2 <- as.numeric(NA)
	}
	else if((missing(mean2) || is.null(mean2)) && (missing(mean.alt) || is.null(mean.alt))) {
		compute.what <- "delta"
		compute.function <- "compute.delta"
		delta <- as.numeric(NA)
		mean2 <- as.numeric(NA)
	}
	else {
		compute.what <- "power"
		compute.function <- "compute.power"
		power <- as.numeric(NA)
	}
	if(compute.what != "delta") {
		if(!(missing(mean2) || is.null(mean2))) {
			if(missing(one.sample)) {
				one.sample <- FALSE
			}
		}
		else if(!(missing(mean.alt) || is.null(mean.alt))) {
			mean2 <- mean.alt
			if(missing(one.sample)) {
				one.sample <- TRUE
			}
		}
		else {
			stop(paste("A second (alternative) mean is required to compute", compute.what))
		}
	}
	else if(missing(one.sample)) {
		one.sample <- missing(n2) && missing(prop.n2)
	}
	if(one.sample) {
		arg.names <- c("mean1", "sd1", "mean2", "delta", "alpha", "power", "n1")
		table.names <- c("mean.null", "sd1", "mean.alt", "delta", "alpha", "power", "n1")
		n2 <- NULL
		prop.n2 <- NULL
		sd2 <- NULL
	}
	else {
		table.names <- c("mean1", "sd1", "mean2", "sd2", "delta", "alpha", "power", "n1", "n2", "prop.n2")
		arg.names <- table.names
	}
	power.table <- build.power.table(theta1 = mean, disp1 = sd1, theta2 = mean2, disp2 = sd2, alpha = alpha, power = power, n1 = n1, n2 = n2, prop.n2 = prop.n2, expand.args = expand.args, one.sample = one.sample, compute.what = compute.what)
	names(power.table) <- arg.names
	#
	###
	### Compute quantiles for alpha and power
	###
	if(alt.expanded == "two.sided") {
		Zalpha <- qnorm(1 - power.table$alpha/2)
	}
	else {
		Zalpha <- qnorm(1 - power.table$alpha)
	}
	if(!all(is.na(power.table$power))) {
		Zpower <- qnorm(power.table$power)
	}
	else {
		Zpower <- as.numeric(NA)
	}
	arglist <- c(power.table, list(Zalpha = Zalpha, Zpower = Zpower, one.sample = one.sample, exact.n = exact.n, alternative = alt.expanded))
	#
	#
	# DEBUG: stop here to make sure arglist is correct
	#
	# return(arglist)
	#
	###
	### use do.call() to compute value and fill into 'compute.what' column 
	###
	if(compute.what == "sample.size") {
		if(one.sample) {
			compute.what <- "n1"
		}
		else {
			compute.what <- c("n1", "n2")
		}
	}
	power.table[, compute.what] <- do.call(compute.function, arglist)
	if(recompute.power && !exact.n && compute.function == "compute.sample.size") {
		if(one.sample) {
			arglist[[compute.what]] <- power.table[, compute.what]
		}
		else {
			arglist[compute.what] <- power.table[, compute.what]
		}
		power.table[, "power"] <- do.call("compute.power", arglist)
	}
	if(compute.function == "compute.delta") {
		power.table$mean2 <- switch(alt.expanded,
			two.sided = ,
			greater = {
				power.table$mean1 + abs(power.table$delta)
			}
			,
			less = {
				power.table$mean1 - abs(power.table$delta)
			}
			)
	}
	names(power.table) <- table.names
	return(power.table)
}



build.power.table <-
function(power, n1, n2, prop.n2, alpha, theta1, theta2, disp1, disp2, expand.args, one.sample, compute.what)
{
	if(!is.null(prop.n2) && any(prop.n2 <= 0)) {
		stop("prop.n2 must be greater than 0")
	}
	if(missing(disp1)) {
		disp1 <- NULL
		disp2 <- NULL
	}
	###
	### If not computing N, need to reconcile n1, n2 and prop.n2.
	### If expand.args, and n1, n2 different lengths, expand the N's.
	###
	if(!(one.sample || compute.what == "sample.size")) {
		expand.n <- length(n1) != length(n2)
		if(expand.args && expand.n) {
			if(is.null(n1)) {
				n.table <- expand.grid(n1 = as.numeric(NA), n2 = n2, prop.n2 = prop.n2)
				n.table$n1 <- n.table$n2/n.table$prop.n2
			}
			else if(is.null(n2)) {
				n.table <- expand.grid(n1 = n1, n2 = as.numeric(NA), prop.n2 = prop.n2)
				n.table$n2 <- n.table$n1 * n.table$prop.n2
			}
			else {
				n.table <- expand.grid(n1 = n1, n2 = n2, prop.n2 = as.numeric(NA))
				n.table$prop.n2 <- n.table$n2/n.table$n1
			}
			prop.n2 <- seq(nrow(n.table))
			n1 <- n2 <- as.numeric(NA)
		}
		else {
			if(is.null(n1)) {
				n1 <- n2/prop.n2
			}
			else if(is.null(n2)) {
				n2 <- prop.n2 * n1
			}
			else {
				prop.n2 <- n2/n1
			}
			n.table <- as.data.frame(list(n1 = n1, n2 = n2, prop.n2 = prop.n2))
			if(expand.args) {
				prop.n2 <- seq(nrow(n.table))
				n1 <- n2 <- as.numeric(NA)
			}
		}
	}
	arg.list <- list(theta1 = theta1, disp1 = disp1, theta2 = theta2, disp2 = disp2, delta = as.numeric(NA), alpha = alpha, power = power, n1 = n1, n2 = n2, prop.n2 = prop.n2)
	arg.list <- arg.list[sapply(arg.list, length) > 0]
	if(expand.args) {
		###
		### Expand table, then put back n1 and n2 if two-sample
		###
		power.table <- expand.grid(arg.list)
		if(!(one.sample || compute.what == "sample.size")) {
			n.table <- n.table[power.table$prop.n2,  ]
			power.table$prop.n2 <- n.table$prop.n2
			power.table$n1 <- n.table$n1
			power.table$n2 <- n.table$n2
		}
	}
	else {
		power.table <- as.data.frame(arg.list)
	}
	if(compute.what != "delta") {
		power.table$delta <- power.table$theta2 - power.table$theta1
	}
	oldClass(power.table) <- c("power.table", "data.frame")
	return(power.table)
}




summary.power.table <-
function(ptable, columns, digits = options()$digits)
{
	if(missing(columns)) {
		columns <- "standard"
	}
	if(is.character(columns)) {
		format <- char.expand(columns, c("standard", "brief", "all"), character(0))
		if(length(format) > 0) {
			if(format == "all") {
				columns <- names(ptable)
			}
			else {
				columns <- c("delta", "power", "n1")
				if(!is.null(ptable$n2)) {
					columns <- c(columns, "n2", "prop.n2")
				}
				if(format == "standard") {
					extra.col <- names(ptable)[is.na(match(names(ptable), columns))]
					if(length(extra.col) == 0) {
						stop("Failed to match standard column names")
					}
					extra.length <- sapply(ptable[, extra.col, drop = FALSE], function(x)
					{
						length(unique(x))
					}
					)
					extra.col <- extra.col[extra.length > 1]
					columns <- c(extra.col, columns)
				}
				if(!is.null(ptable$prop.n2) && all(ptable$prop.n2 == 1)) {
					columns <- columns[columns != "prop.n2"]
				}
			}
		}
	}
	ptable <- ptable[, columns, drop = FALSE]
	digits <- rep(digits, length = ncol(ptable))
	for(i in seq(along = digits)) {
		ptable[, i] <- round(ptable[, i], digits = digits[i])
	}
	return(ptable)
}





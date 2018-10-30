bsp <-
function(p2, p.alt, p = 0.5, power = 0.8, alpha = 0.05, 
   n1 = NULL, n2 = NULL, prop.n2 = 1, one.sample = FALSE, 
   alternative = "two.sided", expand.args = TRUE, 
   exact.n = FALSE, recompute.power = FALSE, correct = TRUE)
{
	compute.sample.size <- function(Zalpha, Zpower, p1, p2, prop.n2, delta, one.sample, exact.n, correct, ...)
	{
		if(one.sample) {
			var.null <- p1 * (1 - p1)
			var.alt <- p2 * (1 - p2)
			prop.n2 <- 1
		}
		else {
			p.pooled <- (p1 + prop.n2 * p2)/(1 + prop.n2)
			var.null <- p.pooled * (1 - p.pooled) * (1 + 1/prop.n2)
			var.alt <- p1 * (1 - p1) + ((p2 * (1 - p2))/prop.n2)
		}
		n <- ((sqrt(var.null) * Zalpha + sqrt(var.alt) * Zpower)/delta)^2
		if(correct) {
			n <- n + (prop.n2 + 1)/(prop.n2 * abs(delta))
		}
		if(!exact.n) {
			n <- ceiling(n)
		}
		if(!one.sample) {
			if(!exact.n) {
				n <- list(n, ceiling(n * prop.n2))
			}
			else {
				n <- list(n, n * prop.n2)
			}
		}
		return(n)
	}

   compute.power <- function(Zalpha, p1, p2, n1, n2, prop.n2, delta, one.sample, correct, alternative, ...)
	{
      # print(list(Zalpha=Zalpha, p1=p1, p2=p2, n1=n1, delta=delta, one.sample=one.sample, correct=correct))
		if(one.sample) {
			var.null <- p1 * (1 - p1)
			var.alt <- p2 * (1 - p2)
			prop.n2 <- 1
		}
		else {
			p.pooled <- (p1 + prop.n2 * p2)/(1 + prop.n2)
			var.null <- p.pooled * (1 - p.pooled) * (1 + 1/prop.n2)
			var.alt <- p1 * (1 - p1) + ((p2 * (1 - p2))/prop.n2)
		}
		if(correct) {
			adj <- (prop.n2 + 1)/(prop.n2 * abs(delta))
			n.ok <- !is.infinite(adj)
			if(any(n1 < adj)) {
				warning(paste("Sample size(s) ", paste(n1[n1 < adj], collapse = ", "), "too small for continuity correction.\nActual power less than reported."))
			}
			n1[n.ok] <- n1[n.ok] - adj[n.ok]
			n1 <- pmax(0, n1)
		}

      power <- switch(alternative,
			greater = ifelse(delta == 0, 1 - pnorm(Zalpha),   pnorm((delta * sqrt(n1) - sqrt(var.null) * Zalpha)/sqrt(var.alt))),
			less = ifelse(delta == 0, 1 - pnorm(Zalpha), pnorm(( - delta * sqrt(n1) - sqrt(var.null) * Zalpha)/sqrt(var.alt))),
			two.sided = ifelse(delta == 0, 1 - pnorm(Zalpha), pnorm((delta * sqrt(n1) - sqrt(var.null) * Zalpha)/sqrt(var.alt))) + 
            ifelse(delta == 0, 1 - pnorm(Zalpha),     pnorm(( - delta * sqrt(n1) - sqrt(var.null) * Zalpha)/sqrt(var.alt))))
		return(power)
	}

   compute.delta <- function(Zalpha, Zpower, p1, n1, n2, prop.n2, one.sample, alternative, ...)
	{
		var.null <- (p1 * (1 - p1))/n1
		if(!one.sample) {
			var.null <- var.null * (1 + 1/prop.n2)
		}
		delta <- sqrt(var.null) * (Zalpha + Zpower)
		if(alternative == "less") {
			delta <- ( - delta)
		}
		return(delta)
	}

   if(!missing(alternative)) {
		alt.expanded <- char.expand(alternative, c("two.sided", "greater", "less"), stop("argument 'alternative' must match one of \"greater\", \"less\", \n\t\t\t\t\"two.sided\"."))
	} else {
		alt.expanded <- alternative
	}

   if(is.null(n1) && is.null(n2)) {
		compute.what <- "sample.size"
		compute.function <- "compute.sample.size"
		n1 <- n2 <- as.numeric(NA)
	} else if((missing(p2) || is.null(p2)) && (missing(p.alt) || is.null(p.alt))) {
		compute.what <- "delta"
		compute.function <- "compute.delta"
		delta <- as.numeric(NA)
		p2 <- as.numeric(NA)
	}
	else {
		compute.what <- "power"
		compute.function <- "compute.power"
		power <- as.numeric(NA)
	}
	if(compute.what != "delta") {
		if(!(missing(p2) || is.null(p2))) {
			if(missing(one.sample)) {
				one.sample <- FALSE
			}
		}
		else if(!(missing(p.alt) || is.null(p.alt))) {
			p2 <- p.alt
			if(missing(one.sample)) {
				one.sample <- TRUE
			}
		}
		else {
			stop(paste("A second (alternative) proportion is required to compute", compute.what))
		}
	} else if(missing(one.sample)) {
		one.sample <- missing(n2) && missing(prop.n2)
	}
	if(one.sample) {
		arg.names <- c("p1", "p2", "delta", "alpha", "power", "n1")
		table.names <- c("p.null", "p.alt", "delta", "alpha", "power", "n1")
		n2 <- NULL
		prop.n2 <- NULL
	} else {
		table.names <- c("p1", "p2", "delta", "alpha", "power", "n1", "n2", "prop.n2")
		arg.names <- table.names
	}
	power.table <- build.power.table(theta1 = p, theta2 = p2, alpha = alpha, power = power, n1 = n1, n2 = n2, prop.n2 = prop.n2, expand.args = expand.args, one.sample = one.sample, compute.what = compute.what)
	names(power.table) <- arg.names

   if(alt.expanded == "two.sided") {
		Zalpha <- qnorm(1 - power.table$alpha/2)
	} else {
		Zalpha <- qnorm(1 - power.table$alpha)
	}
	if(!all(is.na(power.table$power))) {
		Zpower <- qnorm(power.table$power)
	} else {
		Zpower <- as.numeric(NA)
	}
	arglist <- c(power.table, list(Zalpha = Zalpha, Zpower = Zpower, one.sample = one.sample, exact.n = exact.n, correct = correct, alternative = alt.expanded))

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
		power.table$p2 <- switch(alt.expanded,
			two.sided = ,
			greater = {
				power.table$p1 + abs(power.table$delta)
			}
			,
			less = {
				power.table$p1 - abs(power.table$delta)
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


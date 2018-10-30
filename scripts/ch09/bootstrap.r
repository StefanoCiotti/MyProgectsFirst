bootstats <-
function(replicates, observed, n, call, seed.start, seed.end, dim.obs = NULL, group = NULL, indices = NULL)
{
	## Calculate bootstrap statistics and create a "bootstrap" object.
	# Note statistics make no adjustments based on group.  If such
	# adjustments are needed they are currently left to the user.
	if(any(is.na(replicates))) {
		warning("NA's encountered in replicates.  Mean and standard error are calculated using remaining observations")
		B.missing <- sum(rowSums(is.na(replicates)) > 0)
		boot.mean <- colMeans(replicates, na.rm = T)
		boot.se <- sqrt(colVars(replicates, na.rm = T))
	}
	else {
		B.missing <- NULL
		boot.mean <- colMeans(replicates)
		boot.se <- sqrt(colVars(replicates))
	}
	if(all(boot.se == 0) && replicates[1] != 1)
		warning("Bootstrap replicates are identical; does your statistic depend on the resampled data?  In some cases bootstrap(..., assign.frame1=T) helps.")
	boot.bias <- boot.mean - observed
	est <- data.frame(Bias = boot.bias, Mean = boot.mean, SE = boot.se)
	B <- dim(replicates)[1]
	result <- list(call = call, observed = observed, replicates = replicates, estimate = est, B = B, n = n, dim.obs = dim.obs, group = group, seed.start = seed.start, seed.end = seed.end, B.missing = B.missing, indices = indices)
	# Components after seed.end may be NULL
	oldClass(result) <- c("bootstrap", "resamp")
	result
}

bootstrap <-
function(data, statistic, B = 1000, args.stat = NULL, 
   group = NULL, sampler = samp.boot.mc, 
   seed = .Random.seed, sampler.setup, 
   sampler.wrapup = resamp.return.seed, 
   block.size = min(100, B), trace = T, assign.frame1 = F, 
   save.indices = F, statistic.is.random, seed.statistic = 500)
{
	# Capture call.
	func.call <- match.call()
	# Record unevaluated data and statistic as in the call.
	substitute.stat <- substitute(statistic)
	substitute.data <- substitute(data)
	# If statistic isn't function or name of function or expression, 
	# store it as an expression to pass to fit.func.
	if(is.na(match(mode(substitute.stat), c("name", "function")))) statistic <- substitute.stat
	# Get name of data.
	data.name <- ifelse(is.name(substitute.data), deparse(substitute.data), "data")
	# Get function to evaluate the statistic given data and indices.
	is.df.data <- is.data.frame(data)
	fit.func <- resamp.get.fit.func(statistic, substitute.stat, data.name, is.df.data, is.null(args.stat), assign.frame1, length(dim(data)))
	# Faster subscripting for a data frame, allow duplicate row names
	if(is.df.data && is.null(attr(data, "dup.row.names"))) attr(data, "dup.row.names") <- T
	# Set seed in case statistic uses randomization
	seed <- eval(seed)
	if(missing(statistic.is.random) || statistic.is.random) {
		set.seed(seed.statistic)
		prev.seed <- .Random.seed
	}
	#  Get parameter values for observed data.
	if(assign.frame1) on.exit(if(exists(data.name, frame = 1)) remove(data.name, frame = 1))
	n <- dim(data)[1]
	if(is.null(n))
		n <- length(data)
	observed <- fit.func(1:n, data, statistic, args.stat)
	# Determine if statistic uses randomization; this may fail if
	#  a statistic sometimes use randomization.
	if(missing(statistic.is.random)) statistic.is.random <- any(.Random.seed != prev.seed)
	if(statistic.is.random)
		seed.statistic <- .Random.seed
	# Check that observed is vector or matrix.  The need for a vector
	# or matrix arises due to the use of apply to return a vector or vectorized matrix.
	if(is.null(observed)) stop("Statistic returned a NULL result on observed data.  It must return a vector or matrix.")
	if(!is.atomic(observed))
		stop("Statistic must return a vector or matrix.")
	# Getting parameter names and coercing matrix to vector.
	names.observed <- resamp.get.dimnames(observed, substitute.stat)
	dim.obs <- dim(observed)
	if(!is.null(dim.obs))
		observed <- as.vector(observed)
	names(observed) <- names.observed
	# Keep this in for only one version, to warn users who have their own
	# sampler function that expects a vector as the first argument.
	if(missing(sampler.setup))
		sampler.setup <- function(seed)
		{
			set.seed(seed)
			.Random.seed
		}
	seed.start <- sampler.setup(seed)
	must.swap <- statistic.is.random & any(.Random.seed != seed.statistic)
	# Need to swap only if both the sampler and statistic use .Random.seed
	call.stat <- function(i, fit.func, data, statistic, args.stat, inds.mat)
	fit.func(inds.mat[, i], data, statistic, args.stat)
	if(!missing(group)) {
		# Find group using model.frame() stuff when have data frame.
		# Note this doesn't apply for matrix or vector.
		if(is.df.data) {
			m <- list(as.name("model.frame.default"), data = func.call$data, group = func.call$group)
			mode(m) <- "call"
			m <- eval(m, sys.parent())
			group <- model.extract(m, group)
		}
		# Get indices.
		group.inds <- split(1:n, group)
		ngroup <- length(group.inds)
		ngroups <- unlist(lapply(group.inds, length))
	}
	reps <- matrix(NA, length(observed), sum(B))
	if(save.indices)
		all.indices <- matrix(as.integer(0), n, sum(B))
	nB <- length(B)
	on.exit(if(totalB) {
		cat("\nDid ", totalB, " replications, saving results in .bootstrap.partial.results, interrupt again to abort completely.\n")
		reps <- t(reps[, 1:totalB, drop = F])
		dimnames(reps) <- list(NULL, names.observed)
		func.call$B <- c(B[1:iB][ - iB], doneB)
		seed.end <- "Unknown, due to interrupt"
		assign(".bootstrap.partial.results", where = 1, immediate = T, bootstats(replicates = reps, observed = observed, n = n, call = func.call, seed.start = seed.start, seed.end = seed.end, dim.obs = dim.obs, group = group, indices = if(save.indices) all.indices))
	}
	, add = T)
	for(iB in seq(nB)) {
		nblocks <- ceiling(B[iB]/block.size)
		temp <- 1:block.size
		B2 <- block.size
		inds.mat <- matrix(as.integer(0), n, B2)
		previousB <- sum(B[1:iB]) - B[iB]
		for(i in 1:nblocks) {
			doneB <- (i - 1) * block.size
			totalB <- previousB + doneB
			if(i == nblocks)
				if(B[iB] %% block.size) {
					B2 <- B[iB] %% block.size
					temp <- temp[1:B2]
					inds.mat <- inds.mat[, temp, drop = F]
				}
			if(trace)
				cat("Forming replications ", totalB + 1, " to ", totalB + B2, "\n")
			if(missing(group))
				inds.mat[] <- sampler(n, B2)
			else for(si in 1:ngroup)
					inds.mat[group.inds[[si]],  ] <- group.inds[[si]][sampler(ngroups[si], B2)]
			if(must.swap) {
				seed.sampler <- .Random.seed
				.Random.seed <<- seed.statistic
			}
			tempReps <- lapply(temp, call.stat, fit.func, data, statistic, args.stat, inds.mat)
			if(any(unlist(lapply(tempReps, length)) != length(observed)))
				stop("statistic returns result with varying length")
			reps[, totalB + temp] <- unlist(tempReps)
			if(save.indices)
				all.indices[, totalB + temp] <- inds.mat
			if(must.swap) {
				seed.statistic <- .Random.seed
				.Random.seed <<- seed.sampler
			}
		}
	}
	reps <- t(reps)
	# Assign dimnames
	dimnames(reps) <- list(NULL, names.observed)
	seed.end <- sampler.wrapup()
	on.exit()
	# remove only the most recent on.exit expression
	if(trace) cat("\n")
	bootstats(replicates = reps, observed = observed, n = n, call = func.call, seed.start = seed.start, seed.end = seed.end, dim.obs = dim.obs, group = group, indices = if(save.indices) all.indices)
}


colVars <- 
function(x, narm = TRUE, ss = FALSE, unbiased = TRUE) {
   y<-as.matrix(x)
	answer<-vector()
   n <- length(x[1,])
   if(ss) {
      for (i in 1:n) answer[i]<-var(y[,i],na.rm=narm) * (n - 1)
   }
   else{
      if (unbiased) correct <- 1 else correct <- (n - 1)/n
      for (i in 1:n) answer[i]<-var(y[,i],na.rm=narm) * correct
   }
   answer
}


jack.after.bootstrap <-
function(boot.obj, functional = mean, threshold = 2, ..., frame.eval.boot = sys.parent(1))
{
	# Performs jackknife-after-bootstrap to obtain information on some
	# functional of the bootstrap distribution.  Returns estimates of the
	# functional, its standard error, and measures of the influence of each
	# observation.  
	# The standard error estimates tend to be too large.  I'm interested in
	# finding a well-supported alternative, probably involving weighting.
	# Hardwired options functional="Bias", "Mean", "SE".
	# Otherwise functional is a function.
	if(!inherits(boot.obj, "bootstrap")) stop("boot.obj must be a 'bootstrap' object.")
	func.call <- match.call()
	func.call$functional <- substitute(functional)
	n <- boot.obj$n
	inds <- 1:n
	n.param <- length(boot.obj$observed)
	# Get functional corresponding to "Mean", "Bias", or "SE".
	if(is.character(functional)) functional <- switch(functional,
			Mean = ,
			mean = function(x)
			mean(x, na.rm = T),
			Bias = ,
			bias = function(x)
			mean(x, na.rm = T),
			SE = ,
			se = function(x)
			sqrt(colVars(x, na.rm = T)),
			stop("functional must be a function or a character string 'Bias', 'Mean', or 'SE'."))
	# Get resampling indices, locate matches.
	inds.mat <- resamp.get.dimnames(boot.obj, frame.eval.boot)
	has.match <- function(samp, inds, w)
	duplicated(c(samp, inds))[w]
	matches.mat <- apply(inds.mat, 2, has.match, inds, w = (n + 1):(n + n))
	jabB <- ncol(matches.mat) - rowSums(matches.mat)
	if(any(jabB == 0))
		stop("At least one jackknife-after-bootstrap sample has no replications, cannot calculate influence.  Increase B or use a different sampler.")
	if(any(jabB < 5))
		warning("At least one jackknife-after-bootstrap sample is of size < 5.")
	# Calculate functional for full sample
	func.full <- apply(boot.obj$replicates, 2, functional, ...)
	# Calculate functional for jab samples; loop over parameters
	func.vals <- matrix(nrow = n, ncol = n.param)
	jack.boot <- function(in.samp, reps, func, ...)
	func(reps[!in.samp], ...)
	for(j in 1:n.param)
		func.vals[, j] <- apply(matches.mat, 1, jack.boot, boot.obj$replicates[, j], functional, ...)
	# Corrections if functional is "Bias".
	if(is.character(func.call$functional) && is.element(func.call$functional, c("Bias", "bias"))) {
		boot.call <- boot.obj$call
		jack.call <- call("jackknife", data = boot.call$data, statistic = boot.call$statistic, args.stat = boot.call$args.stat, seed = boot.obj$seed.end, assign.frame1 = ifelse(is.null(boot.call$assign.frame1), F, boot.call$assign.frame1))
		jack.obj <- eval(jack.call, frame.eval.boot)
		func.vals <- func.vals - jack.obj$replicates
		func.full <- func.full - boot.obj$observed
	}
	# Calculate the SE(s) of the functional.
	if(all(is.na(func.vals))) stop("All calculated values are missing; this could occur if there are missing values in the bootstrap replicates and your functional does not omit missing values")
	func.se <- sqrt((n - 1)/n * colVars(func.vals, SumSquares = T, na.rm = T))
	# Calculate jackknife influence values.
	rel.influence <- ( - (n - 1)) * scale(func.vals, center = colMeans(func.vals, na.rm = T), scale = sqrt(n) * func.se)
	# Fiddle with names.
	names(func.se) <- names(boot.obj$observed)
	dimnames(func.vals) <- list(inds, names(boot.obj$observed))
	dimnames(rel.influence) <- dimnames(func.vals)
	# Summary of relative influences.
	lri.func <- function(x, rel.inf, thresh)
	rel.inf[abs(rel.inf[, x]) >= thresh, x, drop = F]
	large.rel.influence <- lapply(names(boot.obj$observed), lri.func, rel.influence, threshold)
	names(large.rel.influence) <- names(func.se)
	# Return results.
	result <- list(call = func.call, functional = data.frame(Func = func.full, SE.Func = func.se), rel.influence = rel.influence, large.rel.influence = large.rel.influence, values.functional = func.vals, dim.obs = boot.obj$dim.obs, threshold = threshold, jabB = jabB)
	oldClass(result) <- "jack.after.bootstrap"
	result
}


jackknife <-
function(data, statistic, args.stat = NULL, seed = .Random.seed, group.size = 1, assign.frame1 = F)
{
	# Delete-one jackknifing.
	# If a group.size is specified then the sample is broken into subsets of
	# the specified size and each if these is jackknifed.  This is similar to
	# partial delete-d jackknifing but no adjustments are made to the resulting
	# statistics.
	# Capture call.
	func.call <- match.call()
	# Record unevaluated data and statistic as in the call.
	substitute.stat <- substitute(statistic)
	substitute.data <- substitute(data)
	# If statistic isn't function or name of function or expression, 
	# store it as an expression to pass to fit.func.
	if(is.na(match(mode(substitute.stat), c("name", "function")))) statistic <- substitute.stat
	# Get name of data.
	data.name <- ifelse(length(substitute.data) == 1, deparse(substitute.data), "data")
	# Get function to evaluate the statistic given data and indices.
	is.df.data <- is.data.frame(data)
	fit.func <- resamp.get.fit.func(statistic, substitute.stat, data.name, is.df.data, is.null(args.stat), assign.frame1, length(dim(data)))
	# Faster subscripting for a data frame, allow duplicate row names
	if(is.df.data) attr(data, "dup.row.names") <- T
	# Set seed in case statistic uses randomization
	if(!missing(seed)) {
		orig.random.seed <- .Random.seed
		on.exit(.Random.seed <<- orig.random.seed)
		set.seed(seed)
	}
	#  Get parameter values for observed data.
	if(assign.frame1) on.exit(if(exists(data.name, frame = 1)) remove(data.name, frame = 1))
	n <- numRows(data)
	observed <- fit.func(1:n, data, statistic, args.stat)
	# Check that observed is vector or matrix.  The need for a vector
	# or matrix arises due to the use of apply to return a vector or vectorized matrix.
	if(is.null(observed)) stop("Statistic returned a NULL result on observed data.  It must return a vector or matrix.")
	if(!is.atomic(observed))
		stop("Statistic must return a vector or matrix.")
	# Getting parameter names and coercing matrix to vector.
	names.observed <- resamp.get.dimnames(observed, substitute.stat)
	dim.obs <- dim(observed)
	if(!is.null(dim.obs))
		observed <- as.vector(observed)
	names(observed) <- names.observed
	# This implementation will delete groups of size group.size.  If group.size
	# does not evenly divide n, some points will never be left out.  The main
	# motivation for the different group.size is in approximating acceleration
	# without doing a full delete-1 jackknife.
	n.groups <- floor(n/group.size)
	drop.inds <- if(group.size == 1)  - (1:n) else split(sample( - (1:n), n.groups * group.size, replace = F), rep(1:n.groups, each = group.size))
	reps <- matrix(unlist(lapply(drop.inds, fit.func, data, statistic, args.stat)), ncol = length(observed), dimnames = list(NULL, names.observed), byrow = T)
	# Get stats.  jackstats() assigns class c("jackknife","resamp")
	jackstats(reps, observed, n.groups, func.call, seed, dim.obs)
}


jackstats <-
function(replicates, observed, n, call, seed.start, dim.obs = NULL)
{
	if(any(is.na(replicates))) {
		assign(".bootstrap.replicates", replicates, where = 1, immediate = T)
		stop("NA's encountered in replicates.  Replicates stored as .bootstrap.replicates.")
	}
	jack.mean <- colMeans(replicates)
	jack.se <- sqrt((n - 1) * colVars(replicates, unbiased = F))
	jack.bias <- (n - 1) * (jack.mean - observed)
	est <- data.frame(Bias = jack.bias, Mean = jack.mean, SE = jack.se)
	B <- dim(replicates)[1]
	result <- list(call = call, observed = observed, replicates = replicates, estimate = est, B = B, n = n, dim.obs = dim.obs, seed.start = seed.start)
	oldClass(result) <- c("jackknife", "resamp")
	result
}

limits.bca <-
function(boot.obj, probs = c(25, 50, 950, 975)/1000, details = F, z0 = NULL, acceleration = NULL, group.size = NULL, frame.eval.jack = sys.parent(1))
{
	# Calculate BCa confidence limits.
	# The user may specify the bias correction z0 and acceleration.  Otherwise
	# jackknifing is used to get the acceleration.  To avoid the full n function
	# evaluations needed for the jackknifing, the user may wish to specify a
	# larger group.size.  By default the group.size is set to yield roughly 20
	# jackknife replicates.
	if(!inherits(boot.obj, "bootstrap")) stop("obj must be a 'bootstrap' object.")
	if(missing(group.size))
		group.size <- max(1, floor(boot.obj$n/20))
	# Get jackknife object so can calculate acceleration.
	accel <- acceleration
	if(is.null(accel)) {
		boot.call <- boot.obj$call
		stratified <- !is.null(boot.call$group)
		if(stratified) {
			if(!missing(group.size) && group.size > 1)
				warning("group.size ignored, due to stratified bootstrap sampling")
			group.size <- 1
		}
		jack.call <- call("jackknife", data = boot.call$data, statistic = boot.call$statistic, args.stat = boot.call$args.stat, seed = boot.obj$seed.end, group.size = group.size, assign.frame1 = ifelse(is.null(boot.call$assign.frame1), F, boot.call$assign.frame1))
		jack.obj <- eval(jack.call, frame.eval.jack)
		if(stratified) {
			group <- boot.obj$group
			group.inds <- split(1:boot.obj$n, group)
			groupSizes <- sapply(group.inds, length)
			names(groupSizes) <- NULL
			groupMeans <- rowsum(jack.obj$replicates, group)/groupSizes
			if(any(cumsum(groupSizes) != sapply(group.inds, max))) {
				# stratification variable not sorted
				inds <- 1:boot.obj$n
				inds[unlist(group.inds, use.names = F)] <- rep(1:length(groupSizes), groupSizes)
			}
			else inds <- rep(1:length(groupSizes), groupSizes)
			adj.rep <- jack.obj$replicates - groupMeans[inds,  ]
		}
		else adj.rep <- jack.obj$replicates - rep(jack.obj$estimate$Mean, each = nrow(jack.obj$replicates))
		accel <- apply(adj.rep, 2, function(x)
		sum(x * x * x)/(-6 * sum(x * x)^1.5))
	}
	# Get zprobs.
	zprobs <- qnorm(probs)
	# Get z0.
	if(is.null(z0)) z0 <- qnorm(colMeans(boot.obj$replicates < rep(boot.obj$observed, each = nrow(boot.obj$replicates)), na.rm = T))
	if(any(abs(z0) > 0.25))
		warning("z0 is outside range (-.25, .25), this indicates extreme bias, assumptions underlying BCa interval may be violated")
	names(z0) <- names(boot.obj$observed)
	# Get BCA percentiles.
	emp.probs <- bca.percent <- matrix(nrow = length(boot.obj$observed), ncol = length(probs))
	for(i in 1:length(boot.obj$observed)) {
		emp.probs[i,  ] <- pnorm(z0[i] + (z0[i] + zprobs)/(1 - accel[i] * (z0[i] + zprobs)))
		bca.percent[i,  ] <- quantile(boot.obj$replicates[, i], emp.probs[i,  ], na.rm = T)
	}
	dimnames(bca.percent) <- list(names(boot.obj$observed), paste(100 * probs, "%", sep = ""))
	if(!details)
		return(bca.percent)
	dimnames(emp.probs) <- dimnames(bca.percent)
	list(limits = bca.percent, emp.probs = emp.probs, z0 = z0, acceleration = accel, group.size = group.size)
}


limits.emp <-
function(x, probs = c(25, 50, 950, 975)/1000)
{
	# Calculate empirical percentiles for replicates in "resamp" object.
	resamp.percentiles <- as.matrix(t(apply(x$replicates, 2, quantile, probs = probs, na.rm = T)))
	dimnames(resamp.percentiles) <- list(dimnames(x$replicates)[[2]], paste(100 * probs, "%", sep = ""))
	resamp.percentiles
}

numRows <-
function(x)
{
  dim(as.matrix(x))[[1]] 
}


plot.jack.after.bootstrap <-
function(x, nrow = NULL, grid.layout = T, id.outliers = T, threshold = x$threshold)
{
	# Produces Cook's distance type of plot showing the influence of each observation
	# upon the functional under consideration.
	n <- dim(x$rel.influence)[1]
	nstats <- ncol(x$rel.influence)
	if(grid.layout && nstats != 1) {
		if(is.null(nrow))
			if(is.null(x$dim.obs)) {
				nrow <- min(nstats, 2)
				old.par <- par(mfrow = c(nrow, ceiling(nstats/nrow)))
			}
			else old.par <- par(mfcol = x$dim.obs)
		else old.par <- par(mfrow = c(nrow, ceiling(nstats/nrow)))
		on.exit(par(old.par))
	}
	ylim <- c(0, max(3, x$rel.influence, na.rm = T))
	for(i in 1:nstats) {
		# Cook's distance type of plot.
		if(all(is.na(x$rel.influence[, i]))) {
			plot(1:n, rep(0, n), type = "h", ylim = ylim, xlab = "Observation", ylab = "Absolute Relative Influence", main = dimnames(x$func)[[1]][i], sub = "All Values are NA")
		}
		else {
			plot(1:n, abs(x$rel.influence[, i]), type = "h", ylim = ylim, xlab = "Observation", ylab = "Absolute Relative Influence", main = dimnames(x$func)[[1]][i])
			abline(h = threshold, lty = 2)
			if(id.outliers) {
				outliers <- abs(x$rel.influence[, i]) >= threshold
				text((1:n)[outliers], abs(x$rel.influence[outliers, i]), (1:n)[outliers])
			}
		}
	}
	invisible(NULL)
}


plot.resamp <-
function(x, nrow = NULL, grid.layout = T, rugplot = F, nclass.func = nclass.FD, bandwidth.func = bandwidth.nrd, ...)
{
	# Histogram of resample values with mean and observed values marked.
	# Includes smooth density estimate and optionally a rug plot of the values.
	nstats <- ncol(x$replicates)
	if(grid.layout && nstats != 1) {
		if(is.null(nrow))
			if(is.null(x$dim.obs)) {
				nrow <- min(nstats, 2)
				old.par <- par(mfrow = c(nrow, ceiling(nstats/nrow)))
			}
			else old.par <- par(mfcol = x$dim.obs)
		else old.par <- par(mfrow = c(nrow, ceiling(nstats/nrow)))
		on.exit(par(old.par))
	}
	# Function to calculate nclass from Venables and Ripley p. 126.
	nclass.FD <- function(x)
	{
		r <- quantile(x, c(0.25, 0.75))
		names(r) <- NULL
		h <- 2 * (r[2] - r[1]) * length(x)^{
			-1/3
		}
		ceiling(diff(range(x))/h)
	}
	for(i in 1:ncol(x$replicates)) {
		xi <- x$replicates[, i]
		xi <- xi[!is.na(xi)]
		if(length(xi) == 0) {
			warning("No nonmissing observations for variable", i)
			next
		}
		hist.vals <- hist(xi, breaks = 'FD', freq = FALSE, plot = F, include.lowest = T, ...)
		dens <- density(xi, width = bandwidth.func(xi), from = hist.vals$breaks[1], to = hist.vals$breaks[length(hist.vals$breaks)], na.rm = T)
		ymax <- max(c(hist.vals$density, dens$y))
      hist(xi, breaks = 'FD', freq = FALSE, plot = T, include.lowest = T, ylim = c(0, ymax), ...)
		line.vals <- approx(dens, xout = c(x$observed[i], x$estimate[i, "Mean"]))
		lines(rep(line.vals$x[1], 2), c(0, line.vals$y[1]))
		lines(rep(line.vals$x[2], 2), c(0, line.vals$y[2]), lty = 2)
		lines(dens)
		if(rugplot)
			rug(xi)
		box()
		if(is.na(match("main", names(list(...)))))
			title(names(x$observed)[i])
	}
	invisible(NULL)
}

print.jack.after.bootstrap <-
function(x, digits = max(options()$digits - 3, 4), ...)
{
	old.digits <- options(digits = 4)
	on.exit(options(old.digits))
	cat("Call:\n")
	print(x$call)
	cat("\nFunctional Under Consideration:\n")
	print(x$call$functional)
	cat("\nFunctional of Bootstrap Distribution of Parameters:\n")
	print(x$functional)
	cat("\nObservations with Large Influence on Functional:\n")
	print(x$large.rel.influence)
	if(any(x$jabB < 100)) {
		cat("Number of jackknife-after-bootstrap samples for each observation:\n")
		print(matrix(x$jab, nrow = 1, dimnames = list("", 1:length(x$jabB))))
	}
	invisible(x)
}


print.resamp <-
function(x, digits = max(options()$digits - 3, 4), ...)
{
	old.digits <- options(digits = digits)
	on.exit(options(old.digits))
	cat("Call:\n")
	print(x$call)
	cat("\nNumber of Replications:", x$B, "\n")
	if(!is.null(x$B.missing))
		cat("\nReplicates with missing values:", x$B.missing, "\n")
	cat("\nSummary Statistics:\n")
	print(cbind(Observed = x$observed, x$estimate))
	invisible(x)
}


print.summary.bootstrap <-
function(x, digits = max(options()$digits - 3, 4), ...)
{
	old.digits <- options(digits = digits)
	on.exit(options(old.digits))
	cat("Call:\n")
	print(x$call)
	cat("\nNumber of Replications:", x$B, "\n")
	if(!is.null(x$B.missing))
		cat("\nReplicates with missing values:", x$B.missing, "(this may cause substantial bias).\n")
	cat("\nSummary Statistics:\n")
	print(cbind(Observed = x$observed, x$estimate))
	cat("\nEmpirical Percentiles:\n")
	print(x$limits.emp)
	cat("\nBCa Confidence Limits:\n")
	print(x$limits.bca)
	if(length(x$correlation) > 1) {
		cat("\nCorrelation of Replicates:\n")
		print(x$correlation)
	}
	invisible(x)
}


qqnorm.resamp <-
function(x, nrow = NULL, grid.layout = T, lines = T, ...)
{
	# qqnorm plots of resample replicates.
	nstats <- ncol(x$replicates)
	if(grid.layout && nstats != 1) {
		if(is.null(nrow))
			if(is.null(x$dim.obs)) {
				nrow <- min(nstats, 2)
				old.par <- par(mfrow = c(nrow, ceiling(nstats/nrow)))
			}
			else old.par <- par(mfcol = x$dim.obs)
		else old.par <- par(mfrow = c(nrow, ceiling(nstats/nrow)))
		on.exit(par(old.par))
	}
	for(i in 1:ncol(x$replicates)) {
		xi <- x$replicates[, i]
		xi <- xi[!is.na(xi)]
		if(length(xi) == 0) {
			warning("No nonmissing observations for variable", i)
			next
		}
		qqnorm(xi, ylab = "replicate quantiles", xlab = 'normal quantiles', main= '', ...)
		if(lines)
			qqline(xi)
		box()
		title(paste('QQ ', names(x$observed)[i]))
	}
	invisible(NULL)
}


resamp.get.dimnames <-
function(observed, stat)
{
	# Form dimnames for matrix of replicates.
	if(is.name(stat)) stat.name <- deparse(stat) else stat.name <- "Param"
	dim.obs <- dim(observed)
	if(is.null(dim.obs)) {
		# Observed is a vector.
		if(is.null(names(observed))) paste(stat.name, if(length(observed) > 1) 1:length(observed), sep = "") else names(observed)
	}
	else {
		# Observed is a matrix.
		obs.dimnames <- dimnames(observed)
		f <- function(dn, n)
		{
			if(length(dn))
				abbreviate(dn, 5)
			else seq(n)
		}
		paste(if(is.null(obs.dimnames)) paste(stat.name, seq(dim.obs[1]), sep = "") else f(obs.dimnames[[1]], dim.obs[1]), rep(f(obs.dimnames[[2]], dim.obs[2]), each = dim.obs[1]), sep = ".")
	}
}


resamp.get.fit.func <-
function(statistic, substitute.stat, data.name, is.df.data, is.null.args.stat, assign.frame1 = F, dimLength)
{
	# Construct a function which takes arguments
	#  inds:      indices for subscripting
	#  data:      vector, matrix, array, or data frame
	#  statistic: function or expression
	#  args.stat: optional arguments or objects (may be NULL)
	ffs <- paste("function(inds, data, statistic, args.stat){", "data <- data[inds", if(dimLength > 1) paste(c(rep(",", dimLength), "drop=F"), collapse = ""), "]\n")
	if(assign.frame1)
		ffs <- paste(ffs, "assign('", data.name, "', data, frame=1)\n", sep = "")
	# Handle function, or expression
	if(is.function(statistic)) ffs <- paste(ffs, if(is.null.args.stat) "statistic(data)}" else "do.call('statistic',c(list(data), args.stat))}") else ffs <- paste(ffs, "eval(statistic, c(list(", data.name, "=data)", if(is.df.data) ", data", if(!is.null.args.stat) ", args.stat", "))}")
	eval(parse(text = ffs))
}


samp.boot.mc <-
function(n, B)
{
	# Simple Monte Carlo bootstrap.  Each column is one sample.
	x <- sample(n, B * n, replace = T)
	dim(x) <- c(n, B)
	x
}


resamp.return.seed <-
function() .Random.seed


summary.bootstrap <-
function(object, probs = c(25, 50, 950, 975)/1000, frame.eval.jack = sys.parent(1), ...)
{
	out <- list(call = object$call, B = object$B, observed = object$observed, estimate = object$estimate, limits.emp = limits.emp(object, probs), limits.bca = limits.bca(object, probs, frame.eval.jack = frame.eval.jack, ...), correlation = cor(object$replicates, use = "complete.obs"))
	if(!is.null(object$B.missing))
		out$B.missing <- sum(rowSums(is.na(object$replicates)) > 0)
	oldClass(out) <- "summary.bootstrap"
	out
}


summary.resamp <-
function(object, probs = c(25, 50, 950, 975)/1000)
{
	# Summary method for "resamp" objects.
	# Currently used just for "jackknife" objects.
	out <- list(call = object$call, B = object$B, observed = object$observed, estimate = object$estimate, limits.emp = limits.emp(object, probs), correlation = cor(object$replicates))
	oldClass(out) <- "summary.resamp"
	out
}

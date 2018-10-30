balanced.resample <- 
function(n, R)
{
	# Each column is one sample.
	x <- sample(rep(1:n, R), replace = FALSE)
	dim(x) <- c(n, R)
	x
}


boot <-
function(data, statistic, arguments = NULL, R = 1000, 
   strata = NULL, resampling = simple.resample, 
   seed = as.integer(runif(1, -10^8, 10^8)), resampling.wrapup = resamp.return.seed, 
   block.size = min(100, R), trace = TRUE, assign.frame1 = FALSE, 
   save.indices = FALSE, statistic.is.random, seed.statistic = 500)
{
	func.call <- match.call()
	substitute.stat <- substitute(statistic)
	substitute.data <- substitute(data)
	if(is.na(match(mode(substitute.stat), c("name", "function")))) statistic <- substitute.stat
	data.name <- ifelse(is.name(substitute.data), deparse(substitute.data), "data")
	is.df.data <- is.data.frame(data)
	fit.func <- resamp.get.fit.func(statistic, substitute.stat, data.name, is.df.data, is.null(arguments), assign.frame1, length(dim(data)))
	if(is.df.data && is.null(attr(data, "dup.row.names"))) attr(data, "dup.row.names") <- TRUE
	seed <- eval(seed)
	if(missing(statistic.is.random) || statistic.is.random) {
		set.seed(seed.statistic)
		prev.seed <- .Random.seed
	}
	if(assign.frame1) on.exit(if(exists(data.name, frame = 1)) remove(data.name, frame = 1))
	n <- dim(data)[1]
	if(is.null(n))
		n <- length(data)
	observed <- fit.func(1:n, data, statistic, arguments)
	if(missing(statistic.is.random)) statistic.is.random <- any(.Random.seed != prev.seed)
	if(statistic.is.random)
		seed.statistic <- .Random.seed
	if(is.null(observed)) stop("Statistic returned a NULL result on observed data.  It must return a vector or matrix.")
	if(!is.atomic(observed))
		stop("Statistic must return a vector or matrix.")
	names.observed <- resamp.get.dimnames(observed, substitute.stat)
	dim.obs <- dim(observed)
	if(!is.null(dim.obs))
		observed <- as.vector(observed)
	names(observed) <- names.observed
	resampling.setup <- function(seed)
	{
		set.seed(seed)
		.Random.seed
	}
	seed.start <- resampling.setup(seed)
	must.swap <- statistic.is.random & any(.Random.seed != seed.statistic)
	call.stat <- function(i, fit.func, data, statistic, arguments, inds.mat)
	fit.func(inds.mat[, i], data, statistic, arguments)
	if(!missing(strata)) {
		if(is.df.data) {
			m <- list(as.name("model.frame.default"), data = func.call$data, strata = func.call$strata)
			mode(m) <- "call"
			m <- eval(m, sys.parent())
			strata <- model.extract(m, strata)
		}
		strata.inds <- split(1:n, strata)
		ngroup <- length(strata.inds)
		ngroups <- unlist(lapply(strata.inds, length))
	}
	reps <- matrix(NA, length(observed), sum(R))
	if(save.indices)
		all.indices <- matrix(as.integer(0), n, sum(R))
	nR <- length(R)
	on.exit(if(totalR) {
		cat("\nDid ", totalR, " replications, saving results in .bootstrap.partial.results, interrupt again to abort completely.\n")
		reps <- t(reps[, 1:totalR, drop = FALSE])
		dimnames(reps) <- list(NULL, names.observed)
		func.call$R <- c(R[1:iR][ - iR], doneR)
		seed.end <- "Unknown, due to interrupt"
		assign(".bootstrap.partial.results", where = 1, immediate = TRUE, bootstats(resamples = reps, observed = observed, n = n, call = func.call, seed.start = seed.start, seed.end = seed.end, dim.obs = dim.obs, strata = strata, indices = if(save.indices) all.indices))
	}
	, add = TRUE)
	for(iR in seq(nR)) {
		nblocks <- ceiling(R[iR]/block.size)
		temp <- 1:block.size
		R2 <- block.size
		inds.mat <- matrix(as.integer(0), n, R2)
		previousR <- sum(R[1:iR]) - R[iR]
		for(i in 1:nblocks) {
			doneR <- (i - 1) * block.size
			totalR <- previousR + doneR
			if(i == nblocks)
				if(R[iR] %% block.size) {
					R2 <- R[iR] %% block.size
					temp <- temp[1:R2]
					inds.mat <- inds.mat[, temp, drop = FALSE]
				}
			if(trace)
				cat("Forming replications ", totalR + 1, " to ", totalR + R2, "\n")
			if(missing(strata))
				inds.mat[] <- resampling(n, R2)
			else for(si in 1:ngroup)
					inds.mat[strata.inds[[si]],  ] <- strata.inds[[si]][resampling(ngroups[si], R2)]
			if(must.swap) {
				seed.resampling <- .Random.seed
				.Random.seed <<- seed.statistic
			}
			tempReps <- lapply(temp, call.stat, fit.func, data, statistic, arguments, inds.mat)
			if(any(unlist(lapply(tempReps, length)) != length(observed)))
				stop("statistic returns result with varying length")
			reps[, totalR + temp] <- unlist(tempReps)
			if(save.indices)
				all.indices[, totalR + temp] <- inds.mat
			if(must.swap) {
				seed.statistic <- .Random.seed
				.Random.seed <<- seed.resampling
			}
		}
	}
	reps <- t(reps)
	dimnames(reps) <- list(NULL, names.observed)
	seed.end <- resampling.wrapup()
	on.exit()
	if(trace) cat("\n")
	bootstats(resamples = reps, observed = observed, n = n, call = func.call, seed.start = seed.start, seed.end = seed.end, dim.obs = dim.obs, strata = strata, indices = if(save.indices) all.indices)
}


bootstats <-
function(resamples, observed, n, call, seed.start, seed.end, dim.obs = NULL, strata = NULL, indices = NULL)
{
	if(any(is.na(resamples))) {
		warning("NA's in resamples.")
		R.missing <- sum(rowSums(is.na(resamples)) > 0)
		boot.mean <- colMeans(resamples, na.rm = TRUE)
		boot.se <- sqrt(colVars(resamples, na.rm = TRUE))
	}
	else {
		R.missing <- NULL
		boot.mean <- colMeans(resamples)
		boot.se <- sqrt(colVars(resamples))
	}
	if(all(boot.se == 0) && resamples[1] != 1)
		warning("Replicates are identical. Vefrify that the statistic depends on the resampled data.")
	boot.bias <- boot.mean - observed
	est <- data.frame(Bias = boot.bias, Mean = boot.mean, SE = boot.se)
	R <- dim(resamples)[1]
	result <- list(call = call, observed = observed, resamples = resamples, estimate = est, R = R, n = n, dim.obs = dim.obs, strata = strata, seed.start = seed.start, seed.end = seed.end, R.missing = R.missing, indices = indices)
	oldClass(result) <- c("boot", "resamp")
	result
}


colVars <- 
function(x,   na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE) {
   y<-as.matrix(x)
	answer<-vector()
   n <- length(x[1,])
   if(SumSquares) {
      for (i in 1:n) answer[i]<-var(y[,i],na.rm=na.rm) * (n - 1)
   }
   else{
      if (unbiased) correct <- 1 else correct <- (n - 1)/n
      for (i in 1:n) answer[i]<-var(y[,i],na.rm=na.rm) * correct
   }
   answer
}


jack.after.boot <-
function(boot.obj, functional = mean, threshold = 2, ..., frame.eval.boot = sys.parent(1))
{
	if(!inherits(boot.obj, "boot")) stop("boot.obj must be a 'boot' object.")
	func.call <- match.call()
	func.call$functional <- substitute(functional)
	n <- boot.obj$n
	inds <- 1:n
	n.param <- length(boot.obj$observed)
	if(is.character(functional)) functional <- switch(functional,
			Mean = ,
			mean = function(x)
			mean(x, na.rm = TRUE),
			Bias = ,
			bias = function(x)
			mean(x, na.rm = TRUE),
			SE = ,
			se = function(x)
			sqrt(colVars(x, na.rm = TRUE)),
			stop("functional must be a function or a character string 'Bias', 'Mean', or 'SE'."))
	inds.mat <- resamp.get.indices(boot.obj, frame.eval.boot)
	has.match <- function(samp, inds, w)
	duplicated(c(samp, inds))[w]
	matches.mat <- apply(inds.mat, 2, has.match, inds, w = (n + 1):(n + n))
	jabR <- ncol(matches.mat) - rowSums(matches.mat)
	if(any(jabR == 0))
		stop("At least one jackknife-after-bootstrap sample has no replications, cannot calculate influence.  Increase R or use a different resampling.")
	if(any(jabR < 5))
		warning("At least one jackknife-after-bootstrap sample is of size < 5.")
	func.full <- apply(boot.obj$resamples, 2, functional, ...)
	func.vals <- matrix(nrow = n, ncol = n.param)
	jack.boot <- function(in.samp, reps, func, ...)
	func(reps[!in.samp], ...)
	for(j in 1:n.param)
		func.vals[, j] <- apply(matches.mat, 1, jack.boot, boot.obj$resamples[, j], functional, ...)
	if(is.character(func.call$functional) && is.element(func.call$functional, c("Bias", "bias"))) {
		boot.call <- boot.obj$call
		jack.call <- call("jackknife", data = boot.call$data, statistic = boot.call$statistic, arguments = boot.call$arguments, seed = boot.obj$seed.end, assign.frame1 = ifelse(is.null(boot.call$assign.frame1), FALSE, boot.call$assign.frame1))
		jack.obj <- eval(jack.call, frame.eval.boot)
		func.vals <- func.vals - jack.obj$resamples
		func.full <- func.full - boot.obj$observed
	}
	if(all(is.na(func.vals))) stop("All calculated values are missing; this could occur if there are missing values in the bootstrap resamples and your functional does not omit missing values")
	func.se <- sqrt((n - 1)/n * colVars(func.vals, SumSquares = TRUE, na.rm = TRUE))
	rel.influence <- ( - (n - 1)) * scale(func.vals, center = colMeans(func.vals, na.rm = TRUE), scale = sqrt(n) * func.se)
	names(func.se) <- names(boot.obj$observed)
	dimnames(func.vals) <- list(inds, names(boot.obj$observed))
	dimnames(rel.influence) <- dimnames(func.vals)
	lri.func <- function(x, rel.inf, thresh)
	rel.inf[abs(rel.inf[, x]) >= thresh, x, drop = FALSE]
	large.rel.influence <- lapply(names(boot.obj$observed), lri.func, rel.influence, threshold)
	names(large.rel.influence) <- names(func.se)
	result <- list(call = func.call, functional = data.frame(Func = func.full, SE.Func = func.se), rel.influence = rel.influence, large.rel.influence = large.rel.influence, values.functional = func.vals, dim.obs = boot.obj$dim.obs, threshold = threshold, jabR = jabR)
	oldClass(result) <- "jack.after.boot"
	result
}


jackknife <-
function(data, statistic, arguments = NULL, seed = .Random.seed, strata.size = 1, assign.frame1 = FALSE)
{
	func.call <- match.call()
	substitute.stat <- substitute(statistic)
	substitute.data <- substitute(data)
	if(is.na(match(mode(substitute.stat), c("name", "function")))) statistic <- substitute.stat
	data.name <- ifelse(length(substitute.data) == 1, deparse(substitute.data), "data")
	is.df.data <- is.data.frame(data)
	fit.func <- resamp.get.fit.func(statistic, substitute.stat, data.name, is.df.data, is.null(arguments), assign.frame1, length(dim(data)))
	if(is.df.data) attr(data, "dup.row.names") <- TRUE
	if(!missing(seed)) {
		orig.random.seed <- .Random.seed
		on.exit(.Random.seed <<- orig.random.seed)
		set.seed(seed)
	}
	if(assign.frame1) on.exit(if(exists(data.name, frame = 1)) remove(data.name, frame = 1))
	n <- numRows(data)
	observed <- fit.func(1:n, data, statistic, arguments)
	if(is.null(observed)) stop("Statistic returned a NULL result on observed data.  It must return a vector or matrix.")
	if(!is.atomic(observed))
		stop("Statistic must return a vector or matrix.")
	names.observed <- resamp.get.dimnames(observed, substitute.stat)
	dim.obs <- dim(observed)
	if(!is.null(dim.obs))
		observed <- as.vector(observed)
	names(observed) <- names.observed
	n.groups <- floor(n/strata.size)
	drop.inds <- if(strata.size == 1)  - (1:n) else split(sample( - (1:n), n.groups * strata.size, replace = FALSE), rep(1:n.groups, each = strata.size))
	reps <- matrix(unlist(lapply(drop.inds, fit.func, data, statistic, arguments)), ncol = length(observed), dimnames = list(NULL, names.observed), byrow = TRUE)
	jackstats(reps, observed, n.groups, func.call, seed, dim.obs)
}


jackstats <-
function(resamples, observed, n, call, seed.start, dim.obs = NULL)
{
	if(any(is.na(resamples))) {
		assign(".boot.resamples", resamples, where = 1, immediate = TRUE)
		stop("NA's encountered in resamples.  Replicates stored as .boot.resamples.")
	}
	jack.mean <- colMeans(resamples)
	jack.se <- sqrt((n - 1) * colVars(resamples, unbiased = FALSE))
	jack.bias <- (n - 1) * (jack.mean - observed)
	est <- data.frame(Bias = jack.bias, Mean = jack.mean, SE = jack.se)
	R <- dim(resamples)[1]
	result <- list(call = call, observed = observed, resamples = resamples, estimate = est, R = R, n = n, dim.obs = dim.obs, seed.start = seed.start)
	oldClass(result) <- c("jackknife", "resamp")
	result
}


bca <-
function(boot.obj, p = c(25, 50, 950, 975)/1000, details = FALSE, z0 = NULL, acceleration = NULL, strata.size = NULL, frame.eval.jack = sys.parent(1))
{
	if(!inherits(boot.obj, "boot")) stop("obj must be a 'boot' object.")
	if(missing(strata.size))
		strata.size <- max(1, floor(boot.obj$n/20))
	accel <- acceleration
	if(is.null(accel)) {
		boot.call <- boot.obj$call
		stratified <- !is.null(boot.call$strata)
		if(stratified) {
			if(!missing(strata.size) && strata.size > 1)
				warning("strata.size ignored, due to stratified bootstrap sampling")
			strata.size <- 1
		}
		jack.call <- call("jackknife", data = boot.call$data, statistic = boot.call$statistic, arguments = boot.call$arguments, seed = boot.obj$seed.end, strata.size = strata.size, assign.frame1 = ifelse(is.null(boot.call$assign.frame1), FALSE, boot.call$assign.frame1))
		jack.obj <- eval(jack.call, frame.eval.jack)
		if(stratified) {
			strata <- boot.obj$strata
			strata.inds <- split(1:boot.obj$n, strata)
			groupSizes <- sapply(strata.inds, length)
			names(groupSizes) <- NULL
			groupMeans <- rowsum(jack.obj$resamples, strata)/groupSizes
			if(any(cumsum(groupSizes) != sapply(strata.inds, max))) {
				inds <- 1:boot.obj$n
				inds[unlist(strata.inds, use.names = FALSE)] <- rep(1:length(groupSizes), groupSizes)
			}
			else inds <- rep(1:length(groupSizes), groupSizes)
			adj.rep <- jack.obj$resamples - groupMeans[inds,  ]
		}
		else adj.rep <- jack.obj$resamples - rep(jack.obj$estimate$Mean, each = nrow(jack.obj$resamples))
		accel <- apply(adj.rep, 2, function(x)
		sum(x * x * x)/(-6 * sum(x * x)^1.5))
	}
	zprobs <- qnorm(p)
	if(is.null(z0)) z0 <- qnorm(colMeans(boot.obj$resamples < rep(boot.obj$observed, each = nrow(boot.obj$resamples)), na.rm = TRUE))
	if(any(abs(z0) > 0.25))
		warning("z0 is outside range (-.25, .25), this indicates extreme bias, assumptions underlying bca interval may be violated")
	names(z0) <- names(boot.obj$observed)
	emp.probs <- bca.percent <- matrix(nrow = length(boot.obj$observed), ncol = length(p))
	for(i in 1:length(boot.obj$observed)) {
		emp.probs[i,  ] <- pnorm(z0[i] + (z0[i] + zprobs)/(1 - accel[i] * (z0[i] + zprobs)))
		bca.percent[i,  ] <- quantile(boot.obj$resamples[, i], emp.probs[i,  ], na.rm = TRUE)
	}
	dimnames(bca.percent) <- list(names(boot.obj$observed), paste(100 * p, "%", sep = ""))
	if(!details)
		return(bca.percent)
	dimnames(emp.probs) <- dimnames(bca.percent)
	list(limits = bca.percent, emp.probs = emp.probs, z0 = z0, acceleration = accel, strata.size = strata.size)
}


limits.emp <-
function(x, p = c(25, 50, 950, 975)/1000)
{
	resamp.percentiles <- as.matrix(t(apply(x$resamples, 2, quantile, p = p, na.rm = TRUE)))
	dimnames(resamp.percentiles) <- list(dimnames(x$resamples)[[2]], paste(100 * p, "%", sep = ""))
	resamp.percentiles
}

numRows <-
function(x)
{
  dim(as.matrix(x))[[1]] 
}


plot.jack.after.boot <-
function(x, nrow = NULL, grid.layout = TRUE, id.outliers = TRUE, threshold = x$threshold)
{
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
	ylim <- c(0, max(3, x$rel.influence, na.rm = TRUE))
	for(i in 1:nstats) {
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
function(x, nrow = NULL, grid.layout = TRUE, rugplot = FALSE, nclass.func = nclass.FD, bandwidth.func = bandwidth.nrd, ...)
{
	nstats <- ncol(x$resamples)
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
	nclass.FD <- function(x)
	{
		r <- quantile(x, c(0.25, 0.75))
		names(r) <- NULL
		h <- 2 * (r[2] - r[1]) * length(x)^{
			-1/3
		}
		ceiling(diff(range(x))/h)
	}
   
   xlab <-  dimnames(x$resamples)[[2]]
	for(i in 1:ncol(x$resamples)) {
		xi <- x$resamples[, i]
		xi <- xi[!is.na(xi)]
		if(length(xi) == 0) {
			warning("No nonmissing observations for variable", i)
			next
		}
		hist.vals <- hist(xi, breaks = 'FD', freq = FALSE, plot = FALSE, include.lowest = TRUE, ...)
		dens <- density(xi, width = bandwidth.func(xi), from = hist.vals$breaks[1], to = hist.vals$breaks[length(hist.vals$breaks)], na.rm = TRUE)
		ymax <- max(c(hist.vals$density, dens$y))
      hist(xi, breaks = 'FD', freq = FALSE, include.lowest = TRUE, ylim = c(0, ymax), xlab = xlab[i], ...)
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

print.jack.after.boot <-
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
	if(any(x$jabR < 100)) {
		cat("Number of jackknife-after-bootstrap samples for each observation:\n")
		print(matrix(x$jab, nrow = 1, dimnames = list("", 1:length(x$jabR))))
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
	cat("\nReplications:", x$R, "\n")
	if(!is.null(x$R.missing))
		cat("\nReplicates with missing values:", x$R.missing, "\n")
	cat("\nStatistics:\n")
	print(cbind(Observed = x$observed, x$estimate))
	invisible(x)
}


print.summary.boot <-
function(x, digits = max(options()$digits - 3, 4), ...)
{
	old.digits <- options(digits = digits)
	on.exit(options(old.digits))
	cat("Call:\n")
	print(x$call)
	cat("\nReplications:", x$R, "\n")
	if(!is.null(x$R.missing))
		cat("\nReplicates with missing values:", x$R.missing, "(this may cause substantial bias).\n")
	cat("\nStatistics:\n")
	print(cbind(Observed = x$observed, x$estimate))
	cat("\nEmpirical percentiles:\n")
	print(x$limits.emp)
	cat("\nbca confidence limits:\n")
	print(x$bca)
	if(length(x$correlation) > 1) {
		cat("\nCorrelation of Replicates:\n")
		print(x$correlation)
	}
	invisible(x)
}


qqnorm.resamp <-
function(x, nrow = NULL, grid.layout = TRUE, lines = TRUE, ...)
{
	nstats <- ncol(x$resamples)
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
	for(i in 1:ncol(x$resamples)) {
		xi <- x$resamples[, i]
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
	if(is.name(stat)) stat.name <- deparse(stat) else stat.name <- "Param"
	dim.obs <- dim(observed)
	if(is.null(dim.obs)) {
		if(is.null(names(observed))) paste(stat.name, if(length(observed) > 1) 1:length(observed), sep = "") else names(observed)
	}
	else {
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
function(statistic, substitute.stat, data.name, is.df.data, is.null.arguments, assign.frame1 = FALSE, dimLength)
{
	ffs <- paste("function(inds, data, statistic, arguments){", "data <- data[inds", if(dimLength > 1) paste(c(rep(",", dimLength), "drop=FALSE"), collapse = ""), "]\n")
	if(assign.frame1)
		ffs <- paste(ffs, "assign('", data.name, "', data, frame=1)\n", sep = "")
	if(is.function(statistic)) ffs <- paste(ffs, if(is.null.arguments) "statistic(data)}" else "do.call('statistic',c(list(data), arguments))}") else ffs <- paste(ffs, "eval(statistic, c(list(", data.name, "=data)", if(is.df.data) ", data", if(!is.null.arguments) ", arguments", "))}")
	eval(parse(text = ffs))
}


resamp.get.indices <-
function(boot.obj, frame.eval.boot = sys.parent(1))
{
	if(!is.null(boot.obj$indices)) return(boot.obj$indices)
	boot.call <- boot.obj$call
	boot.call$statistic <- function(...)
	1
	boot.call$save.indices <- TRUE
	boot.call$seed <- boot.obj$seed.start
	boot.call$trace <- FALSE
	eval(boot.call, frame.eval.boot)$indices
}


resamp.return.seed <-
function() runif(1, -10^8,10^8)


summary.boot <-
function(object, p = c(25, 50, 950, 975)/1000, frame.eval.jack = sys.parent(1), ...)
{
	out <- list(call = object$call, R = object$R, observed = object$observed, estimate = object$estimate, limits.emp = limits.emp(object, p), bca = bca(object, p, frame.eval.jack = frame.eval.jack, ...), correlation = cor(object$resamples, use = "complete.obs"))
	if(!is.null(object$R.missing))
		out$R.missing <- sum(rowSums(is.na(object$resamples)) > 0)
	oldClass(out) <- "summary.boot"
	out
}


simple.resample <-
function(n, R)
{
	x <- sample(n, R * n, replace = TRUE)
	dim(x) <- c(n, R)
	x
}



summary.resamp <-
function(object, p = c(25, 50, 950, 975)/1000)
{
	out <- list(call = object$call, R = object$R, observed = object$observed, estimate = object$estimate, limits.emp = limits.emp(object, p), correlation = cor(object$resamples))
	oldClass(out) <- "summary.resamp"
	out
}

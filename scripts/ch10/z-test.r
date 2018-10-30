z.test <-
function(x, mu = 0, conf.level = 0.95){
    if(!missing(mu) && (length(mu) != 1 || is.na(mu)))
        stop("'mu' must be a single number")
    if(!missing(conf.level) &&
       (length(conf.level) != 1 || !is.finite(conf.level) ||
        conf.level < 0 || conf.level > 1))
        stop("'conf.level' must be between 0 and 1")
   mx <- mean(x, na.rm = TRUE)
   vx <- var(x, na.rm = TRUE)
   n <- length(x)
   stderr <- sqrt(vx/n)
   if(stderr < 10 *.Machine$double.eps * abs(mx))
      stop("data are essentially constant")
   return(round(c(pvalue=pnorm(mx,mu,stderr)),3))
}
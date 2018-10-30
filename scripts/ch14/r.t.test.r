r.t.test <- function(x, df, alternative = c("two.sided", 
  "less", "greater"), sig.level = 0.05, ...){
   alternative <- match.arg(alternative)

   if(!missing(sig.level) && 
    (length(sig.level) != 1 || !is.finite(sig.level) || 
    sig.level < 0 || sig.level > 1))
      stop("sig.level must be a single number between 0 and 1")
   dname <- deparse(substitute(x))
   tstat <- x * sqrt(df) / sqrt(1 -x^2)
   if (alternative == "less") {
      pval <- pt(tstat, df)
   }
   else if (alternative == "greater") {
      pval <- pt(tstat, df, lower = FALSE)
   }
   else {
      pval <- 2 * pt(-abs(tstat), df)
   }
   names(tstat) <- "t"
   names(df) <- "df"
   rval <- data.frame(statistic = round(tstat,3), df = df, 
    p.value = round(pval,3), alternative = alternative,
    name = dname)
   return(rval)
}

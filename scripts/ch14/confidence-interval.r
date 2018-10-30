ci.lm <- function(x, object, alpha = 0.05, 
  plot.it = T, ...) {
   f <- predict(object, se.fit = TRUE)
   p <- length(coef(object))
   fit <- f$fit
   adjust <- (p * qf(1 - alpha, p, length(fit) -
    p))^0.5 * f$se.fit
   lower <- fit - adjust
   upper <- fit + adjust
   if(plot.it) {
      Y <- fit + resid(object)
      plot(x, Y, xlab='x')
      abline(reg = object, lwd = 3,col = 'red')
      ord <- order(fit)
      lines(x[ord], lower[ord],lwd = 2, col = 'red')
      lines(x[ord], upper[ord], lwd = 2, col = 'red')
      invisible(list(lower = lower, upper = upper, 
        col = 'red'))
   }
   else list(lower = lower, upper = upper)
}

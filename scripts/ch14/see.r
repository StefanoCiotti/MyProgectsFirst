see <- function(x, Y, fn){
   model <- lm(Y ~ x)
   print(summary(model))
   print(anova(model))
   openg(5, 2.75)
   par(mfrow = c(1, 2))
   ci.lm(x, model)
   plot(model$fitted.values, rstandard(model),
      xlab='fit', ylab = 'standardized residuals')
   abline(h = 0)
   saveg(fn, 5, 2.75)
   model
}

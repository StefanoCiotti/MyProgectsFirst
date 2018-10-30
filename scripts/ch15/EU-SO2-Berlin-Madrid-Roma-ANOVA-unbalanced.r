rm(list = ls())
load('SO2.rda')
names(SO2)

(n <- tapply(SO2$'Max SO2', SO2$city, length))
d <- SO2

(vars <- tapply(d[, 2], d[, 3], var))
(vars.ratio <- c(vars[1]/vars[2],
  vars[1] / vars[3], vars[3] / vars[2]))
(p.L <- c(pf(vars.ratio[1], 5, 5),
  pf(vars.ratio[2], 5, 5),
  pf(vars.ratio[3], 5, 5)))
  
d[, 2] <- log(d[, 2])
(vars <- tapply(d[, 2], d[, 3], var))
(vars.ratio <- c(vars[1]/vars[2],
  vars[1] / vars[3], vars[3] / vars[2]))
(p.L <- c(pf(vars.ratio[1], 5, 5),
  pf(vars.ratio[2], 5, 5), pf(vars.ratio[3], 5, 5)))
boxplot(d$'Max SO2' ~ d$city)

# The model is unbalanced.
# So SS are not useful, so we use GLT
model <- lm(d$'Max SO2' ~ d$city)
anova(model)
tapply(SO2[, 2], SO2[, 3], mean)
# because SS decomposition are not useful, we mut use GLT
# We accept the one-factor model or the two-factor;
# depending on our judgement.
# For the model we accept we have to check the residuals
openg(4,4)
par(mfrow = c(2, 2))
plot(model)
saveg('EU-SO2-unbalanced-diagnostics', 4, 4)

library(granova)
openg(4.5, 3.5)
granova.1w(d$'Max SO2', d$city)
saveg('EU-SO2-Berlin-Madrid-Roma-ANOVA-unbalanced-plot', 4.5, 3.5)


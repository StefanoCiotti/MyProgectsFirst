rm(list = ls())
par(mfrow = c(1, 2))
options(stringsAsFactors = TRUE)
UN <- read.table('who-population-data-2002.txt',
   header = TRUE, sep = '\t')
names(UN)[c(6, 7, 8, 9, 11)] <- c('% urban', '% growth',
  'birth rate', 'death rate', 'under 5 mortality')
save(UN, file = 'UN.rda')

(X.bar <- mean(UN$'% growth', na.rm = TRUE))
(S <- sd(UN$'% growth', na.rm = TRUE))
Chebyshev <- c(low = X.bar - 4.472 * S,
  high = X.bar + 4.472 * S)
round(Chebyshev, 2)

h(UN$'% growth', xlab = '% growth per year')
x <- seq(-2, 5, length = 201)
lines(x, dnorm(x, X.bar, S))

mu.hat <- X.bar ; sigma.hat <- S
x <- seq(min(UN$'% growth'), max(UN$'% growth'), length = 201)
plot(x, pnorm(x, mu.hat, sigma.hat), type = 'l', 
  xlab = 'quantile (% growth / year)',
  ylab = expression(italic(Phi(x))))
p <- seq(0, 1, length = 51)
q <- quantile(UN$'% growth', probs = p)
points(q, p)

empirical <- c(low = X.bar - 2 * S,
  high = X.bar + 2 * S)
round(rbind(Chebyshev, empirical), 2)

print(UN$country[UN$'% growth' < 0])
length(UN$country[!is.na(UN$'% growth')])

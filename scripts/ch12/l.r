rm(list = ls())
load('l.rda')
head(l)

openg()
boxplot(l$a.lead, l$e.lead, names = c('Africa', 'Europe'))
identify(rep(2, length(l$e.lead)), l$e.lead, 
  labels=l$europe)
saveg('world-bank-lead-in-gasoline')

mu.hat <- c(mean(l$a.lead), mean(l$e.lead, trim = 1/31))
e <- sort(l$e.lead)
e <- e[ -c(1, length(e))]
S <- c(sd(l$a.lead), sd(e))

n = c(length(l$a.lead), length(l$e.lead) - 2)
l.stats <- data.frame(mu.hat, S, n)
dimnames(l.stats)[[1]] <- c('Africa', 'Europe') ; l.stats

SE <- sqrt(sum(S^2 / n)) ; alpha <- 0.05
c(low = qnorm(alpha / 2, mu.hat[2] - mu.hat[1], SE), 
	high = qnorm(1 - alpha / 2,  mu.hat[2] - mu.hat[1], SE))
	
gun.control <- data.frame(n = c(615, 585),
	'in favor' = c(463, 403))
dimnames(gun.control)[[1]] <- c('question A', 'question B')
p.bar <- sum(gun.control[, 2]) / sum(gun.control[, 1])
gun.control <- data.frame(gun.control, p = gun.control[, 2] /
	gun.control[, 1])
print(gun.control)
attach(gun.control)

SE <- sqrt(p.bar * (1 - p.bar) * (sum(1/n)))
print(SE)
Z <- (p[1] - p[2] + (1 / (2 * n[1]) - 1 / (2 * n[2]))) / SE
print(Z)



rm(list = ls())
par(mfrow = c(1, 2))
load('cardiac.rda')
(mu.hat <- mean(cardiac$basebp))
(sigma.hat <- sd(cardiac$basebp))
x <- seq(80, 220, length = 201)
plot(x, pnorm(x, mu.hat, sigma.hat), type = 'l')
p <- seq(0, 1, length = 21)
q <- quantile(cardiac$basebp, probs = p)
points(q, p)
plot(ecdf(cardiac$basebp))
x <- seq(0, 10, length = 101) ; lambda <- 0.5
set.seed(10) ; u <- runif(3) ; r.x <- qexp(u, lambda)
plot(x, pexp(x, lambda), type = 'l', xlim = c(0, 10))
for(i in 1 : 3){
  arrows(-1, u[i], r.x[i], u[i], code = 2,
    length = 0.1, angle = 20)
  arrows(r.x[i], u[i], r.x[i], -0.04, code = 2,
    length = 0.1, angle = 20)
}
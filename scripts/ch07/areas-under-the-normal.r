pg <- function(x, i){
  if (i == 1){
    x1 <- x[x <= -1] ; y1 <- dnorm(x1)
    x2 <- c(-3, x1, x1[length(x1)], -3)
  }
  if (i == 2){
    x1 <- x[x >= 1] ; y1 <- dnorm(x1)
    x2 <- c(1, x1, x1[length(x1)], 1)
  }
  if(i == 3){
    x1 <- x[x >= -1 & x <= 1] ; y1 <- dnorm(x1)
    x2 <- c(-1, x1, 1,  - 1)
  }
  y2 <- c(0, y1, 0, 0)
  polygon(x2, y2, col = 'grey90')
}

xl <- expression(italic(x))
yl <- c(expression(italic(phi(x))), '', '')
m <- c(expression(italic(P(X <= -1))),
  expression(italic(P(X >= 1))),
  expression(paste(italic('P('), italic(-1 <= X), 
  italic(phantom()<= 1), ')', sep = '')))
par(mfrow = c(1, 3))
x <- seq(-3, 3, length = 501) ; y <- dnorm(x)
for(i in 1 : 3){
  plot(x, y, type = 'l', xlab = xl, ylab = yl[i],
    main = m[i]) ; abline(h = 0) ; pg(x, i)
}
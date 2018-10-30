x <- seq(-6, 6, length = 201)
plot(x, dnorm(x), xlab = expression(paste(italic(z), ' or ', 
   italic(t))), ylab = 'density', type = 'l', lwd = 2)
df <- c(1, 3, 12) ; for (i in df) lines(x, dt(x, i))
labels <- c('normal', expression(italic(t[1])), 
   expression(italic(t[3])))
atx <- c(2, 0, 0) ; aty <- c(0.35, 0.26, 0.34) 
text(atx, aty, labels = labels)
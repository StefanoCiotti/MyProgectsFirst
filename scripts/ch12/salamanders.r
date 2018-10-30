salamanders <- data.frame(
   type = c(rep('lungless', 4), rep('lungs', 6)), 
   head = c(0.13, 0.13, 0.16, 0.19, 0.23, 0.23, 0.17, 
      0.16, 0.16, 0.14))
attach(salamanders)
plot(type, head, ylab = 'head width (mm)')
v <- tapply(head, type, var)
n <- c(length(type[type == 'lungless']), 
   length(type[type == 'lungs']))
w <- (n - 1) / (sum(n) - 2)
s <- sum(w * v)
treatment <- c(94, 197, 16, 38, 99, 141, 23)
control <- c(52, 104, 146, 10, 50, 31, 40, 27, 46)
library(simpleboot)
set.seed(10)
b <- two.boot(treatment, control, mean, 
	R = 1500, student = TRUE, M = 50)
b$t0[1]
bci <- boot.ci(b)
bci$normal

hist(b)
abline(v = b$t0[1], col = 'red', lwd = 2)
abline(v = bci$normal[2], lty = 2)
abline(v = bci$normal[3], lty = 2)

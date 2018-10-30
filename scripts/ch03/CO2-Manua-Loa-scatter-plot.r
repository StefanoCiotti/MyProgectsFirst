manua <- scan()
manua.ts <- ts(manua, start = 1958, end = 2002)
plot(manua.ts, xlab = 'year', ylab = expression(CO[2]))
points(manua.ts)
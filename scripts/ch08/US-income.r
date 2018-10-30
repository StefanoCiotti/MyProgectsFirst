load('us.income.rda')
plot(us.income$year, us.income$mean / 1000,
  type = 'l', ylim = c(30, 60), ylab = 'income in $1000')
lines(us.income$year, us.income$median / 1000, lwd = 3)
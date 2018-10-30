load('cancerCrudeRate.rda') ; attach(cancerCrudeRate)
plot(year, lambda, type = 's', 
   ylab = 'crude rate')
lines(year, lower, type = 's')
lines(year, upper, type = 's')
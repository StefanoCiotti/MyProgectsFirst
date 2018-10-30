rm(list=ls())

wb <- read.csv('world-bank.csv', header = TRUE, sep = ',',
  stringsAsFactors = FALSE)
names(wb) <- c('country', 'indicator', '1999', '2000',
   '2001', '2002', '2003')

wb.split <- split(wb[,-2],wb$indicator)
names(wb.split) <- levels(wb$indicator)

out <- function(d, x, y, xlab, ylab, trans = FALSE){
  countries <- intersect(d[[x]][,1], d[[y]][,1])
  index <- vector()
  for(i in 1 : length(countries)){
    index[i] <- which(d[[x]][, 1] == countries[i])
  }
  if(trans) xx <- log(d[[x]][index, 3])
  else xx <- d[[x]][index, 3]
  index <- vector()
  for(i in 1 : length(countries)){
    index[i] <- which(d[[y]][, 1] == countries[i])
  }
  if(trans) yy <- log(d[[y]][index, 3])
  else yy <- d[[y]][index, 3]
  plot(xx, yy, xlab = xlab, ylab = ylab)
  model <- lm(yy ~ xx)
  abline(reg = model)
  print(summary(model))
}

openg(4,4)
par(mfrow=c(2, 2))
# energy use vs CO2 emissions
out(wb.split, 4, 2, 'log(CO2 emissions)',
  'log(energy use)', trans = TRUE)
# GDP vs CO2 emissions
out(wb.split, 2, 6, 'log(CO2 emissions)', 'log(GDP)',
  trans = TRUE)
# fertility rate vs infant mortality
out(wb.split, 5, 8, 'fertility rate', 'life expectancy')
# edu vs infant mortality
out(wb.split, 11, 12, 'infant mortality', 
  'children mortality')
#saveg('world-bank', 4, 4)

print(c(sqrt(0.8415), sqrt(0.2498), sqrt(0.6801), 
  sqrt(0.9766)))
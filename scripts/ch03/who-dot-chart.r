who <- read.csv('who.by.continents.and.regions.txt',
  sep = '\t')
m.region <- tapply(who$dr, who$region, mean, na.rm = TRUE)
dotchart(m.region, xlab = 'death rate')
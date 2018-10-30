who.fertility.mortality <- read.table(
  'who.by.continents.and.regions.txt', sep = '\t',
  header = TRUE)
names(who.fertility.mortality) <- c('country', 'continent',
  'region', 'population', 'density', '% urban', '% growth',
  'birth rate', 'death rate', 'fertility',
  'under 5 mortality')

save(who.fertility.mortality,
  file = 'who.fertility.mortality.rda')
d <- who.fertility.mortality
plot(d[, 'birth rate'], d[, 'death rate'],
  xlab = 'birth rate', ylab = 'death rate')
unusual <- identify(d[, 8], d[, 9], labels = d[, 1])
points(d[unusual, 8], d[unusual, 9], pch = 19)
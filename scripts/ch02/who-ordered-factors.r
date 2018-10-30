rm(list = ls())
options(width = 60)
who <- read.csv('who.by.continents.and.regions.txt',
   sep = '\t', header = TRUE)
names(who)

region.dr <- who[, c(3, 9)]
region <- as.character(region.dr[ , 1])
cont <- reg <- vector()
splt <- strsplit(region, ' ')
for(i in 1 : length(region)) {
   cont[i] <- splt[[i]][2]
   reg[i] <- splt[[i]][1]
}
c.r <- paste(cont, reg,sep = ', ')
new.region.dr <- data.frame(c.r, region.dr[ , 2])
levels(new.region.dr[ , 1])
dotchart(tapply(new.region.dr[ , 2],new.region.dr[ , 1],
   mean, na.rm = TRUE))
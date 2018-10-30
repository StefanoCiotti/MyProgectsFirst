rm(list = ls())
options(width = 60)

load('capital.punishment.rda')
length(capital.punishment[, 1])
idx <- complete.cases(capital.punishment[,
  c('Race', 'Education')])
cp <- capital.punishment[idx, ]
length(cp[, 1])
skin <- cp$Race
education <- cp$Education
levels(skin)
levels(skin)[-2] <- FALSE
levels(skin)[2] <- TRUE
levels(skin)

library(Design)
ddist <- datadist(skin, education)
options(datadist = 'ddist')
(blacks <- lrm(skin ~ education, x = TRUE, y = TRUE))

openg()
plot(blacks, xlab = 'education',
  ylab = 'log odds of skin color')
# saveg('capital-punishment-simple-lr')
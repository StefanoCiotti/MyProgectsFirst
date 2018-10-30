rm(list=ls())
source('ci.lm.R')
source('r.t.test.R')
source('see.R')

height<-c(5+11/12, 5+3/12, 6,6+2/12, 5+6/12)
weight<-c(200, 110, 190, 200, 140)
age<-c(58, 54, 24, 20, 19)

family <- data.frame(height, weight, age)
dimnames(family)[[1]] <- c('father', 'mother', 'son', 
  'son ', 'daughter')
model <- see(height, weight, 'family-linear-model')
sm <- summary(model)
r <- sqrt(sm$r.squared)
df.r <- sm$df[2]
r.t.test(r, df.r)

height <- c(6+1/12, 5+5/12, 5+3/12, 5+8/12, 5+5/12, 5+6/12)
weight <- c(220, 100, 110, 85, 130, 140)
age <- c(80, 78, 54, 52, 48, 46)
another.family <- data.frame(height, weight, age)
dimnames(another.family)[[1]] <- c('father', 'mother',
  'daughter', 'daughter ', 'daughter  ', 'daughter   ')
another.model <- lm(weight ~ height)
openg()
ci.lm(height, another.model)
abline(reg = model, lty = 2)
saveg('another-family-linear-model')


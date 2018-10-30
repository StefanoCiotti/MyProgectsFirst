rm(list = ls())
load('who.fertility.mortality.rda')
d <- who.fertility.mortality
pairs(who.fertility.mortality[, 7 : 9])
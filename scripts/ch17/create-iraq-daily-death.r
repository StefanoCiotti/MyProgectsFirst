rm(list = ls())
load('iraq.dead.rda')
load('iraq.injured.rda')
start <- iraq.dead$Julian[1] ; 
end <- iraq.dead$Julian[length(iraq.dead$Julian)]
j.day <- start : end

# consider dead from hostile only
dead.hostile <- 
  iraq.dead[iraq.dead$Major.Cause.of.Death == 'Hostile', ]
daily.dead <- data.frame(Julian = j.day, 
  Count = rep(0, length(j.day)))
daily.count <- as.data.frame(table(dead.hostile$Julian))
names(daily.count) <- c('Julian', 'Count')

idx <- as.integer(as.character(daily.count$Julian)) - 
  as.integer(as.character(daily.count$Julian[1]))+1

daily.dead[idx, 2] <- daily.count$Count
iraq.daily.deaths <- daily.dead
save(iraq.daily.deaths, file = 'iraq.daily.deaths.rda')
iraq.dead.hostile <- dead.hostile
save(iraq.dead.hostile, file = 'iraq.dead.hostile.rda')

rm(list = ls())

bmr <- read.table('basal-metabolic-rate.txt', 
  sep = '\t', header = TRUE)
save(bmr, file = 'bmr.rda')
bmr$Family[bmr$Family == as.character("")] <- NA
bmr$Species[bmr$Species == as.character("")] <- NA
bmr$M[bmr$M == as.character("")] <- NA
bmr$T[bmr$T == as.character("")] <- NA
bmr$BMR[bmr$BMR == as.character("")] <- NA
save(bmr, file = 'bmr.rda')
idx.remove <- which(is.na(bmr$Family) & is.na(bmr$Species))
bmr <- bmr[-(idx.remove), ]
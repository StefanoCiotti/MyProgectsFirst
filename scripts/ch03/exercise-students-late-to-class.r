late <- c(0, 2, 5, 0, 3, 1, 8, 0, 3, 1, 1, 9, 2, 4, 
   0, 2, 9, 3, 0, 1, 9, 8)
late <- as.data.frame(table(late))
late <- data.frame(late, Dens = late$Freq / sum(late$Freq))
late <- data.frame(late, 'Cum Dens' = cumsum(late$Dens))
options(digits = 3)
print(late)
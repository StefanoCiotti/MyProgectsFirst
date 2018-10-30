composers <-c ('Sybelius', 'Wagner', 'Shostakovitch')
grandiose <- c(1, 3, 2)
(music <- data.frame(composers, grandiose))
music[, 1] <- as.character(music[, 1])

noquote(cbind('BY NAME' = music$composer,
  '|' = '|', 'BY INDEX' = music[, 1],
  '|' = '|', 'BY NAMED-INDEX' = music[, 'composers']))

rm(list = ls())
load('EU.rda')
load('EU.station.rda')

stations <- EU.station[complete.cases(EU.station), ]
stations$country_name <- as.character(stations$country_name)
stations$station_type_of_area <-
  as.character(stations$station_type_of_area)

a <- c('BELGIUM', 'FRANCE', 'GERMANY', 'ITALY', 'SPAIN',
  'UNITED KINGDOM')
b <- c('rural', 'suburban', 'urban')

stations <- stations[is.element(stations[, 3], a), ]
unique(stations[, 3])
stations <- stations[is.element(stations[, 5], b), ]
unique(stations[, 5])

# isolate CO data
EU.CO <- EU[EU[, 3] == 'CO' & EU[, 14] == 'annual mean',
  c(1, 15)]

country <- area <- vector(length = length(EU.CO[, 1]))
for(i in 1 : length(a)){
  s <- stations[stations[, 3] == a[i], 1]
  idx <- is.element(EU.CO[, 1], s)
  country[idx] <- a[i]
}
for(i in 1 : length(b)){
  s <- stations[stations[, 5] == b[i], 1]
  idx <- is.element(EU.CO[, 1], s)
  area[idx] <- b[i]
}

# add country and area to EU.CO
EU.CO <- cbind(EU.CO, country, area)
tmp <- EU.CO[EU.CO[, 3] != FALSE | EU.CO[, 4] != FALSE, ]

# do the same for longitude and latitude
long <- lat <- vector(length = length(tmp[, 1]))
tmp <- cbind(tmp, long = long, lat = lat)
s <- unique(tmp[, 1])
for(i in 1 : length(s)){
  tmp[tmp[, 1] == s[i], 5] <- stations[stations[, 1] ==s[i], 6]
  tmp[tmp[, 1] == s[i], 6] <- stations[stations[, 1] ==s[i], 7]
}


# save
EU.CO <- tmp
EU.CO$country <- as.character(EU.CO$country)
EU.CO$country <- as.factor(EU.CO$country)
EU.CO$area <- as.character(EU.CO$area)
EU.CO$area <- as.factor(EU.CO$area)
names(EU.CO)[1] <- 'station'
names(EU.CO)[2] <- 'CO'
names(EU.CO)[5] <- 'long'
names(EU.CO)[6] <- 'lat'
save(EU.CO, file = 'EU.CO.rda')

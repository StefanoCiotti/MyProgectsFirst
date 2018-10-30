western.africa <- 1 ; northern.europe <- 2
region.name <- c('Western Africa', 'Northern Europe')
file.name <- c('WesternAfrica.tex','NorthernEurope.tex')

# 1. import the data
library(RODBC) ; c <- odbcConnect('who')
sqlTables(c) ; who <- sqlFetch(c, 'MyFormat')
odbcClose(c) ; save(who, file = 'who.fertility.mortality.rda')

# comment/uncomment for desired table 
# region = western.africa
 region = northern.europe

# 2. make data frame
ifelse(region == western.africa, rows <- c(50 : 66),
   rows <- c(135 : 147))
mort <- who$'under 5 mort'[rows]
stats <- c(mean(mort, na.rm = TRUE), 
   var(mort, na.rm = TRUE), 
   sd(mort, na.rm = TRUE))
mort <- data.frame(c(mort, stats))
rnames <- c(as.character(who$country[rows]), 
   '\\hline Mean', 'Variance',
   'Standard deviation')
dimnames(mort) <- list(rnames, c('Mortality'))

# 3. table
library(Hmisc)
cap1 <- paste(region.name[region], 
   ', children (under 5) mortality')
cap2 <- 'per 1000 children under 5.'
latex(mort, file = file.name[region],
   caption = paste(cap1, cap2), 
   label = paste('table:',region.name[region],'mortality'), 
   cdec = 2, 
   rowlabel = 'Country', 
   na.blank = TRUE, 
   where = '!htbp', 
   ctable = TRUE)
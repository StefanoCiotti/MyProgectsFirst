load('who.pop.2000.rda')                   # population data
load('who.ccodes.rda')                       # country codes
load('who.pop.var.names.rda')#variable names in who.pop.2000

cn <- 'Austria'                               # country name
#cn <- 'Armenia'            # uncomment for Armenia bar plot
cc <- who.ccodes$code[who.ccodes$name == cn]  # country code

par(mfrow = c(2, 1))
bl <- as.character(pop.var.names$descr[2 : 26]) # bar labels
gender <- 1                                          # males
rows <- who.pop.2000$code == cc &        # row to be plotted
   who.pop.2000$sex == gender      
columns <- 5 : 29                    # columns to be plotted

barplot(t(who.pop.2000[rows, columns])[, 1]/1000,
   names.arg = bl, main = paste(cn, ', males'), 
   las = 2, col = 'gray90')
gender <- 2                                        # females
rows <- who.pop.2000$code == cc & 
   who.pop.2000$sex == gender
barplot(t(who.pop.2000[rows, columns])[, 1]/1000, 
   names.arg = bl, main = paste(cn, ', females'), 
   las = 2, col = 'gray90')
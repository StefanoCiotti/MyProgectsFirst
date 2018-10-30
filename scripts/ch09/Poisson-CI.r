library(Hmisc) ; library(clim.pact)
load('college.crime') ; cc <- college.crime

sex <- 65 ; cc[cc[, sex]>90, sex] <- NA
assaults <- cc[, sex] / cc$enrollment * 1000
plot(assaults, ylab = 'assualts') ; ID <- identify(assaults)

School <- lower.case(cc[ID, 5])
City <- lower.case(cc[ID, 6])
State <- cc[ID, 7]
collegeSexualAssaults <- data.frame(ID, School, City, 
   State, Assaults = assaults[ID])

c1 <- 'Highest incidences (per 1000 enrolled '
c2 <- 'students) of reported sexual assaults in 1994 '
c3 <- 'among 680 US univresities and colleges.'
latex(collegeSexualAssaults, 
   label = 'table: collegeSexualAssaults', 
   cdec = c(0, 0, 0, 0, 3), 
   col.just = c('r', 'l', 'l', 'l', 'c'), 
   extracolheads = c('', '', '', '', 'per 1000'), 
   rowname = '', rowlabel = '', caption = paste(c1, c2, c3), 
   where = "!htbp", ctable = TRUE)

#chi square ci:
a <- 0.05
d <- 23 # assaults in Cornell
lowCI <- qchisq(a / 2, 2 * d) / 2
highCI <- qchisq(1 - a / 2, 2 * (d + 1)) / 2
print(c(lowCI, highCI))


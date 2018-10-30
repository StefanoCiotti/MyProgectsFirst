load('who.rda')
middle.east.arab<-c(19,72,109,110,117,121,126,129,169,
   171,199,222,237,252)
a<-middle.east.arab
idx=sort(as.character(who$country),index.return=TRUE)
arab.pop=data.frame(country=who$country[idx$ix][a],
   pop=who$pop[idx$ix][a])
arab.pop$pop=as.integer(arab.pop$pop)
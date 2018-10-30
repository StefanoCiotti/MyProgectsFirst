load('faculty.rda')
plot(faculty$height, xlab = 'faculty index', 
	ylab = 'height (ft)')      
identify(faculty, label = faculty$name)
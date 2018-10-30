idx <- sort(faculty$height, decreasing = TRUE, 
   index.return = TRUE)
f <- faculty[idx$ix, ]
windows(width = 3, height = 3, pointsize = 8)
plot(f$height, xlab = 'faculty index', 
   ylab = 'height (ft)')
identify(f$height, label = f$name)
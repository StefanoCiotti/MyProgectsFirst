data(PlantGrowth) ; attach(PlantGrowth)
par(mfrow = c(1, 3))
xl <- c(3, 6.5) ; yl <- c(0, 4)
a <- hist(weight[group == 'ctrl'], xlim = xl, ylim = yl, 
   xlab = '', main = 'control', 
   ylab = 'frequency', col = 'gray90')
b <- hist(weight[group == 'trt1'], xlim = xl, ylim = yl, 
   xlab = 'weight', ylab = '', main = 'treatment 1', 
   col = 'gray90')
c <- hist(weight[group == 'trt2'], xlim = xl, ylim = yl, 
   xlab = '', ylab = '', main = 'treatment 2', 
   col = 'gray90')
source('bootstrapAll.R')

edu <- c(12, 9, 5, 7, 15, 7, 7, 8, 7, 13, 6, 12, 7, 12,
   5, 2, 8, 28, 20, 9, 10, 3, 12, 6, 1, 8, 3, 10, 19,
   8, 2, 6, 2, 6, 3, 9, 3, 13, 12, 11, 13, 32, 7, 7, 53,
   29, 29)
save(edu, file = 'edu.rda')
inc <- 10 + 0.2 * edu + 
   rnorm(edu, 0.01 * mean(edu), 0.01 * var(edu))
d <- data.frame(edu, inc)

bs <- boot(d, var, R=5000)
jab <- jack.after.boot(bs)
print(summary(bs))
print(limits.emp(bs))
print(bca(bs))
print(jab)


#width = 4.5 ; height = 4.5
#openg(width = width, height = height)
plot(bs, main = '', ylab = 'density', col = 'gray90')
# saveg('bootEduHistPDFWMF')

windows()
#openg(width = width, height = height)
qqnorm(bs)
# saveg('bootEduQQPDFWMF')

windows()
#openg(width = width, height = height)
plot(jab)
# saveg('bootEduQQPDFWMF')


detach('package:SADR')
detach('package:MASS')
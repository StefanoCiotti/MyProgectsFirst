rm(list=ls())
load(file = 'temperature.rda')
source('confidence-interval.R')
p <- 2

X <- temperature$Lat
Y <- temperature$JanTemp
n <- length(X)
summary(model <- lm(Y ~ X))

# line and standardized residuals
coast <- c(5, 6, 12, 13, 41, 52, 53)
openg(5, 2.75)
par(mfrow = c(1, 2))
ci.lm(X, model)
points(X[coast], Y[coast], pch = 19)
identify(X, Y)
residuals.standardized <- rstandard(model)
plot(model$fitted.values, residuals.standardized,
   xlab = 'fitted', ylab = 'residuals')
abline(h=0)
points(model$fitted.values[coast],
  residuals.standardized[coast], pch = 19)
text(model$fitted.values[coast],
  residuals.standardized[coast], labels = coast,pos = 4)
identify(model$fitted.values,
  residuals.standardized)
#saveg('temperature-RSTANDARD',5,2.75)

# QQ on standardized residuals
openg()
q <- qqnorm(residuals(model))
identify(q$x, q$y)
qqline(residuals(model))
#saveg('temperature-RSTANDARD-QQ-RESIDUALS')



# standardized vs RSTUDENT
openg()
RSTUDENT<-rstudent(model)
plot(model$fitted.values, RSTUDENT,
   xlab = 'fitted', ylab = 'standard residuals')
abline(h = 0) ; abline(h = 2, lty = 2)
points(model$fitted.values[coast], RSTUDENT[coast],
  pch = 19)
points(model$fitted.values, residuals.standardized, cex = 2)
identify(model$fitted.values, RSTUDENT)
#saveg('temperature-RSTANDARD-vs-RSTUDENT')

# DFFITS
openg()
rdff <- dffits(model)
plot(model$fitted.values, rdff,
   xlab = 'fitted', ylab = 'DFFITS')
abline(h = 0)
abline(h = 3 * sqrt(p / (n - p)), lty = 2)
abline(h = -3 * sqrt(p / (n - p)), lty = 2)
points(model$fitted.values[coast], rdff[coast], pch = 19)
identify(model$fitted.values,rdff)
#saveg('temperature-DFFITS')

# DFBETAS
openg(5,2.75)
par(mfrow=c(1,2))
rdfb<-dfbetas(model)
rdfb0<-rdfb[,1];rdfb1<-rdfb[,2]
plot(model$fitted.values,rdfb0,
   xlab='fitted',ylab='intercept DFBETA')
abline(h=0)
abline(h=1,lty=2)
abline(h=-1,lty=2)
points(model$fitted.values[west],rdfb0[west],pch=19)
identify(model$fitted.values,rdfb0)

plot(model$fitted.values,rdfb1,
   xlab='fitted',ylab='slope DFBETA')
abline(h=0)
abline(h=1,lty=2)
abline(h=-1,lty=2)
points(model$fitted.values[west],rdfb1[west],pch=19)
identify(model$fitted.values,rdfb1)

#saveg('temperature-DFBETAS',5,2.75)

# cooks.distance
openg()
cd<-cooks.distance(model)
plot(cd,
   xlab='observation number',ylab="Cook's distance",
   type='h',ylim=c(0,1))
cutoff<-qf(0.5,p,n-p,lower.tail=FALSE)
abline(h=cutoff,lty=2)
points(west,cd[west],pch=19)
identify(cd)
#saveg('temperature-Cooks-distance')

# influence measures

im<-influence.measures(model)
idx<-list()
for (i in 1:length(im$is.inf[1,])){
   idx[[i]]<-as.data.frame(im$is.inf[im$is.inf[TRUE,i],i])
   idx[[i]]<-dimnames(idx[i])[[1]]
}


m<-im$is.inf[,-4]
v<-round(im$infmat[,-4],3)
cond<-(m[,1]==TRUE | m[,2]==TRUE | m[,3]==TRUE |
   m[,4]==TRUE | m[,5] == TRUE)
tf<-as.data.frame(m[cond,])
idx<-as.numeric(dimnames(tf)[[1]])
sig<-ifelse(tf==TRUE,'*',' ')
is<-v[idx,]
influence<-data.frame(is[,1],sig[,1],is[,2],sig[,2],is[,3],sig[,3],
   is[,4],sig[,4],is[,5],sig[,5])
names(influence)<-c('DFBETA0','SIG','DFBETA1','SIG','DFFIT',
   'SIG','D','SIG','HAT','SIG')
influence

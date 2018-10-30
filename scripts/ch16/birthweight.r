library(SADR)
load('death.rda')
d<-death

# V & R approach:
library(MASS)
options(contrasts=c('contr.treatment','contr.poly'))
attach(birthwt)
race<- factor(race,labels=c('white','black','other'))
print(table(ptl))
ptd<-factor(ptl>0)
print(table(ftv))
ftv<-factor(ftv)
levels(ftv)[-(1:2)]<-'2+'
print(table(ftv))
bwt<-data.frame(low=factor(low),age,lwt,race,
   smoke=(smoke>0),ptd,ht=(ht>0),ui=(ui>0),ftv)
detach(birthwt); rm(race,ptd,ftv)

birthwt.glm<-glm(low~.,family=binomial,data=bwt)
print(summary(birthwt.glm,correlation=FALSE))

birthwt.step<-step(birthwt.glm,trace=FALSE)
print(birthwt.step$anova)

birthwt.step2<-step(birthwt.glm,~.^2,trace=FALSE)
print(birthwt.step2$anova)
print(summary(birthwt.step2)$coeff)

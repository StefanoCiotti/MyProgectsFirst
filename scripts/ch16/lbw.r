load('lbw.rda')

# 
# Run logistic regrssion
#
library(Design)
attach(lbw)
label(LOW)<-'low birth weight'
label(AGE)<-'age'
label(LWT)<-'weight at lat menstrual period'
label(RACE) <-'race'
label(FTV)<-'number of first trimester physician visits'
units(AGE)<-'years'
units(LWT)<-'lbs'
ddist<-datadist(LOW,AGE,LWT,RACE,FTV)
options(datadist='ddist')
model<-lrm(LOW~AGE+LWT+RACE+FTV,
   model=TRUE,x=TRUE,y=TRUE,se.fit=TRUE)

#
# Model without AGE and FTV
#
model1<-lrm(LOW~LWT+RACE,
   model=TRUE,x=TRUE,y=TRUE,se.fit=TRUE)


detach(lbw)

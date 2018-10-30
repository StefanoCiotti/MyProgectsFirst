load('temperature.rda'); attach(temperature,pos=1)
load('remove.rda')
remove=c(remove,53)
tl.all=lm(JanTemp~Lat)
tl.wc.removed=lm(JanTemp[-remove]~Lat[-remove])
tl.wc=lm(JanTemp[remove]~Lat[remove])

windows()
par(cex=2,font=6,font.main=8,font.axis=6,font.lab=8)
rs1=rstandard.lm(tl.all)
plot(rs1,ylab='standardized residuals',main='all data')
abline(h=0)
points(remove,rs1[remove],pch=19)
text(53,rs1[53],labels=as.character(City[53]),pos=2)


windows()
par(cex=2,font=6,font.main=8,font.axis=6,font.lab=8)
qqnorm(rs1,ylab='all residuals')
qqline(rs1)

windows()
par(cex=2,font=6,font.main=8,font.axis=6,font.lab=8)
rs2=rstandard.lm(tl.wc.removed)
plot(rs2,ylab='standardazied residuals',main='west coast cities removed')
abline(h=0)

windows()
par(cex=2,font=6,font.main=8,font.axis=6,font.lab=8)
qqnorm(rs2,ylab='standardized residuals',main='west coast cities removed')
qqline(rs2)

#plot(tl.all,caption='',sub.caption='')
#plot(rstanda

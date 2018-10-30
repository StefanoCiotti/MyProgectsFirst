spreading <- c(
21.0, 26.7, 9.2, 6.7, 29.2, 26.7, 6.7, 8.3, 18.4, 4.9)
spot <- c(
1.6, 4.6, 1.1, 1.2, 21.1, 11.9, 1.8, 4.7, 7.4)

openg(4.5,3)
par(mfrow=c(1,2))
qqnorm(spreading, main = 'spreading') ; qqline(spreading)
qqnorm(spot, main = 'spot') ; qqline(spot)
saveg('exercise-spreading-vs-spot-fires', 4.5,3)

c(var(spreading), var(spot))

t.test(spreading, spot)

shapiro.test(spreading)
shapiro.test(spot)

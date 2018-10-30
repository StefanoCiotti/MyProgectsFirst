rm(list = ls())
load('EU.CO.rda')
EU.CO <- EU.CO[EU.CO$country != 'BELGIUM', ]
attach(EU.CO)

country <- as.character(country[country != 'BELGIUM'])
country <- as.factor(country) ; levels(country)

openg(4, 2.75)
par(mar = c(4, 4, 1, 2))
interaction.plot(area, country, CO, type = 'b')
saveg('EU-CO-two-way-interaction', 4, 2.75)

options(width=60)

lCO <- log(CO + 0.01)
full.model <- lm(lCO ~ country + area + country : area)
anova(full.model)
openg(4, 4)
par(mfrow = c(2, 2))
plot(full.model)
saveg('EU-CO-two-way-diagnostics', 4, 4)

no.inter.model <- lm(lCO ~ country + area)
anova(no.inter.model)
par(mfrow = c(2, 2))
plot(no.inter.model)

summary(aov(CO ~ country))
summary(aov(CO ~ area))
summary(aov(CO ~ country + area))
summary(aov(CO ~ area + country))

full.model <- aov(CO ~ country + area + country : area)
model.tables(full.model, type = 'means')




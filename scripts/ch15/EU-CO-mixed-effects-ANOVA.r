rm(list = ls())
load('EU.CO.rda')
EU.CO <- EU.CO[EU.CO$country != 'BELGIUM', ]
attach(EU.CO)

country <- as.character(country[country != 'BELGIUM'])
country <- as.factor(country) ; levels(country)
logCO <- log(CO + 0.001)

openg(4, 2.75)
par(mar = c(4, 4, 1, 2))
interaction.plot(area, country, logCO, type = 'b')
saveg('EU-log-CO-two-way-interaction', 4, 2.75)

options(width = 60)
library(nlme)
grouped <- groupedData(formula = logCO ~  area | country)
grouped$country <- factor(grouped$country)
grouped$area <- factor(grouped$area)
head(grouped)

library(lattice)
trellis.device(color = FALSE, width = 4.5, height = 4)
xyplot(logCO ~ area | country,
  panel = function(x, y){
    panel.xyplot(x, y)
#    panel.loess(x, y, span = 1)
    panel.lmline(x, y, lty = 2)
  }
)
update(trellis.last.object(),
  par.strip.text = list(cex = 0.75),
  scales = list(cex = 0.6))
saveg('EU-log-CO-trellis', 4.5, 4)

# this allows us to compare means by country and area:
country.list <- lmList(logCO ~ -1 + area | country, data =
  as.data.frame(grouped))
i <- intervals(country.list)
dimnames(i)[[3]] <- c('rural', 'suburban', 'urban')
p <- plot(i) ; p$ylab <- 'log(CO)' ; p
saveg('country-list', 4.5, 4)

boxplot(coef(country.list), names = c('rural', 'suburban',
  'urban'), ylab = 'CO')

model <- lme(logCO ~ area | country, data = grouped)
anova(model)

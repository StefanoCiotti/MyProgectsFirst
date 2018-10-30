years <- c('91-92', '92-93', '95-96')
trapped.adults <- c(49, 60, 25)
resighted.adults <- c(37, 36, 22)
trapped.yearlings <- c(45, 19, 40)
resighted.yearlings <- c(17, 2, 14)

willow.tits <- data.frame(rbind(trapped.adults, 
	resighted.adults, trapped.yearlings,
	resighted.yearlings))
names(willow.tits) <- years
print(willow.tits)

survival.adults <- round(resighted.adults / 
	trapped.adults, 3)
survival.yearlings <- round(resighted.yearlings / 
	trapped.yearlings, 3)
willow.tits <- data.frame(rbind(willow.tits[1:2, ],	
	survival.adults = survival.adults, 
	willow.tits[3:4, ], survival.yearlings = 
		survival.yearlings))
names(willow.tits) <- years
print(willow.tits)

use.normal.approximation <- function(n.S, n){
	p.bar <- sum(n.S) / sum(n)
	if (p.bar * n[1] >=5 & (1 - p.bar) * n[1] >= 5){
		if (p.bar * n[2] >=5 & (1 - p.bar) * n[2] >= 5){
			return(TRUE)
		}
	} else{ 
		return(FALSE)
	}
}

normal.binom <- function(n.S, n){
	p <- n.S / n
	if (p[1] < p[2]){
		temp <- n[1] ; n[1] <- n[2] ; n[2] <- temp
		temp <- n.S[1] ; n.S[1] <- n.S[2] ; n.S[2] <- temp
		p <- n.S / n
	}
	p.bar <- sum(n.S) / sum(n)
	SE <- sqrt(p.bar * (1 - p.bar) * sum(1/n))
	Z <- (p[1] - p[2] + (1 / (2 * n[1]) - 1 / (2 * n[2]))) / SE
	if(Z > qnorm(0.975)) return('reject') else return('accept')
}

adults <- c('91-92 vs 92-93' = 
	use.normal.approximation(willow.tits[2, c(1,2)], 
	willow.tits[1, c(1,2)]),
	'91-92 vs 95-96' = 
	use.normal.approximation(willow.tits[2, c(1,3)], 
	willow.tits[1, c(1,3)]),
	'92-93 vs 95-96' = 
	use.normal.approximation(willow.tits[2, c(2,3)], 
	willow.tits[1, c(2,3)]))
yearlings <- c('91-92 vs 92-93' = 
	use.normal.approximation(willow.tits[5, c(1,2)], 
	willow.tits[4, c(1,2)]),
	'91-92 vs 95-96' = 
	use.normal.approximation(willow.tits[5, c(1,3)], 
	willow.tits[4, c(1,3)]),
	'92-93 vs 95-96' = 
	use.normal.approximation(willow.tits[5, c(2,3)], 
	willow.tits[4, c(2,3)]))
print(rbind(adults, yearlings))

adults <- c('91-92 vs 92-93' = 
	normal.binom(willow.tits[2, c(1,2)], 
	willow.tits[1, c(1,2)]),
	'91-92 vs 95-96' = 
	normal.binom(willow.tits[2, c(1,3)], 
	willow.tits[1, c(1,3)]),
	'92-93 vs 95-96' = 
	normal.binom(willow.tits[2, c(2,3)], 
	willow.tits[1, c(2,3)]))
yearlings <- c('91-92 vs 92-93' = 
	normal.binom(willow.tits[5, c(1,2)], 
	willow.tits[4, c(1,2)]),
	'91-92 vs 95-96' = 
	normal.binom(willow.tits[5, c(1,3)], 
	willow.tits[4, c(1,3)]),
	'92-93 vs 95-96' = 
	normal.binom(willow.tits[5, c(2,3)], 
	willow.tits[4, c(2,3)]))
print(rbind(adults, yearlings))

vn <- read.table('marriage.Viet.Nam.txt', 
	header = TRUE, sep = ',')
set.seed(1)
s <- sample(1 : length(vn[, 1]),5)
print(vn[s, ])

# Do women marry at a younger age?

district <- vn$district[vn$gender == 'women' & 
	vn$age.at == 'marriage']
age <- vn$mean[vn$gender == 'women' & 
	vn$age.at == 'marriage']
S <- vn$sd[vn$gender == 'women' & vn$age.at == 'marriage']
n <- vn$n[vn$gender == 'women' & vn$age.at == 'marriage']

women <- data.frame(district, age, S, n)
p.value <- round(pnorm(women$age, women$age[3], women$S[3] 
	/ sqrt(210)), 3)
women <- data.frame(women, p.value)
print(women)

# Do men engage in sexual intercourse at a younger age?

district <- vn$district[vn$gender == 'men' & vn$age.at == 
	'first intercourse']
age <- vn$mean[vn$gender == 'men' & vn$age.at == 
	'first intercourse']
S <- vn$sd[vn$gender == 'men' & vn$age.at == 
	'first intercourse']
n <- vn$n[vn$gender == 'men' & vn$age.at == 
	'first intercourse']

men <- data.frame(district, age, S, n)
p.value <- round(pnorm(men$age, men$age[3], men$S[3] / 
	sqrt(210)), 3)
men <- data.frame(men, p.value)
print(men)


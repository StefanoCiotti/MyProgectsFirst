load('capital.punishment.rda')
cp <- capital.punishment

blacks <- cp$Race[cp$Race == 'Black']
blacks <- blacks[!is.na(blacks)]
n.B <- length(blacks)
n <- length(cp$Race[!is.na(cp$Race)])

# proportion of blacks in the population of inmates

p <- n.B / n ; p

# from http://www.census.gov/prod/2002pubs/c2kprof00-us.pdf

N <- 281421906
N.B <- 34658190
PI.0 <- N.B / N

# proportion of blacks inmates higher than in the population

dummy.n <- 1000
p.value <- round(1 - pbinom(dummy.n * p, dummy.n, PI.0), 3)

set.seed(1)
i <- sample(1:n, 10)
x <- cp$Race[i]
n.B <- length(x[x == 'Black']) ; n.B
p.value <- round(1 - pbinom(n.B, 10, p), 3) ; p.value

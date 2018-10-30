rm(list = ls())
options(width = 60)
set.seed(1)
midterm <- round(rnorm(25, 75, 15), 0)
final <- round(rnorm(24, 80, 10), 0)
scores <- list(midterm = midterm, final = final)
save(scores, file = 'scores.rda')

# unpaired

rm(list = ls())
load('scores.rda') ; scores

alpha <- 0.05
(v.equal <- var.test(scores$midterm, scores$final))

p.v.equal <- v.equal$p.value
v.equal <- TRUE
if(p.v.equal <= alpha) v.equal <- FALSE
t.test(scores$midterm, scores$final, var.equal = v.equal)

# paired

midterm <- scores$midterm[-1] ; final <- scores$final

t.test(midterm, final, var.equal = 
  (var.test(midterm, final)$p.value <= alpha))



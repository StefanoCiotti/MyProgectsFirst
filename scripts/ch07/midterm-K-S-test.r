midterm <- c(61,  69,  55,  47,  49,  58,  66,  57,  73,
  56,  45,  67,  88,  61,  62,  85,  83,  76,  71,  82,
  84,  73,  86,  81,  57,  74,  89,  71, 93,  59,  89, 108,
  90,  87,  71)
save(midterm, file = 'midterm.rda')

rm(list = ls())
load('midterm.rda')
round( c(mean = mean(midterm), sd = sd(midterm)),1)
ks.test(midterm, 'pnorm', mean(midterm), sd(midterm))
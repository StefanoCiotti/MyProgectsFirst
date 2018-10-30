grades <- read.table('score.csv', sep = ',', header = TRUE)
(grade <- LETTERS[4 : 1])
(letter <- cut(grades$score, breaks = c(60, 70, 80, 90, 101), labels = grade, include.lowest = TRUE, right = FALSE))
grades <- data.frame(grades, letter)
head(grades, 5)


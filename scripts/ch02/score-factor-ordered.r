(grade <- LETTERS[1 : 4])
(grade.factor <- factor(grade))
(grade.ordered <- factor(grade, ordered = TRUE))
is.ordered(grade.factor)
is.ordered(grade.ordered)
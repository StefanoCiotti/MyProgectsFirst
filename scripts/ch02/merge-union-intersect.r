rm(list = ls())
a <- data.frame(letter = LETTERS[1 : 6])
a <- data.frame(a, code = apply(a, 1, utf8ToInt))
b <- data.frame(letter = LETTERS[4 : 9])
b <- data.frame(b, code = apply(b, 1, utf8ToInt))
cbind(a, '|' = '|', b)

merge(a, b)

union(a, b)

cbind(intersect(a, b), '|' = '|', intersect(b, a))

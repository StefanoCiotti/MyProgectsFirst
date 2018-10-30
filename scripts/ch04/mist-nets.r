p <- t(expand.grid(letters[1:4], 1:4))
no.dimnames(noquote(p))

A <- no.dimnames(noquote(p[, p[1, ] == 'a'])) ; A
B <- no.dimnames(noquote(p[, p[2, ] == 1])) ; B

noquote(intersect(A, B))

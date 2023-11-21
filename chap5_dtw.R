
t1 <- c(1, 2, 3, 3, 4, 1)
t2 <- c(1, 1, 3, 4, 3, 1)

## Find the best match with the canonical recursion formula
library(dtw)

alignment <- dtw(t1, t2, keep=TRUE)

x <- alignment$costMatrix
print(x)
print(alignment$distance)
print(alignment$index1)
print(alignment$index2)

t1 <- c(1, 3, 2, 4, 3)
t2 <- c(1, 2, 3, 3, 4)

## Find the best match with the canonical recursion formula
library(dtw)

alignment <- dtw(t1, t2, keep=TRUE)

x <- alignment$costMatrix
print(x)
print(alignment$distance)
print(alignment$index1)
print(alignment$index2)



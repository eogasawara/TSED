library(dtw)

# Set default chunk behavior: no messages or warnings in the output
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

library(dtw)

# Define two short sequences
t1 <- c(1, 2, 3, 3, 4, 1)
t2 <- c(1, 1, 3, 4, 3, 1)
# Compute alignment using standard dynamic programming
# - keep = TRUE stores costMatrix and index mapping for inspection
alignment_a <- dtw(t1, t2, keep = TRUE)
# The cost matrix accumulates local distances along all possible paths
cost_a <- alignment_a$costMatrix
# Overall DTW distance (lower implies better alignment)
alignment_a$distance
# Index mappings along the optimal warping path
# - index1: positions in t1
# - index2: matched positions in t2
alignment_a$index1
alignment_a$index2
# Visualize the warping path over the cost matrix
plot(alignment_a, type = "density", main = "DTW Cost Matrix and Warping Path (Example A)")

# Define a second pair with different local variations
t1 <- c(1, 3, 2, 4, 3)
t2 <- c(1, 2, 3, 3, 4)
# Compute alignment and inspect key outputs again
alignment_b <- dtw(t1, t2, keep = TRUE)
alignment_b$distance
alignment_b$index1
alignment_b$index2
plot(alignment_b, type = "density", main = "DTW Cost Matrix and Warping Path (Example B)")

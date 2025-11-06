library(tspredit)
library(daltoolbox)



i <- seq(0, 2 * pi + 8 * pi / 50, pi / 50)
x <- cos(i)
# Additive Gaussian noise
noise <- rnorm(length(x), mean = 0, sd = sd(x) / 10)
x <- x + noise
# Inject three spike-like anomalies for visualization
set.seed(42)
x[30] <- rnorm(1, 0, sd(x))
x[60] <- rnorm(1, 0, sd(x))
x[90] <- rnorm(1, 0, sd(x))

plot(i, x)
lines(i, x)

sw_size <- 10
xw <- ts_data(x, sw_size)
idx_full <- 1:length(x)

plot(x = idx_full, y = x, main = "")
lines(x = idx_full, y = x, col = "black")
# Show original windows (green)
for (j in 1:nrow(xw)) {
  lines(x = j:(j + sw_size - 1), y = xw[j, 1:sw_size], col = "green")
}
# Fit jitter augmentation and overlay augmented windows
augment <- ts_aug_jitter()
augment <- fit(augment, xw)
xa <- transform(augment, xw)
idx <- attr(xa, "idx")
for (j in 1:nrow(xa)) {
  lines(x = idx[j]:(idx[j] + sw_size - 1), y = xa[j, 1:sw_size], col = "green")
}

plot(x = idx_full, y = x, main = "")
lines(x = idx_full, y = x, col = "black")
for (j in 1:nrow(xw)) {
  lines(x = j:(j + sw_size - 1), y = xw[j, 1:sw_size], col = "green")
}
augment <- ts_aug_awareness(0.25)
augment <- fit(augment, xw)
xa <- transform(augment, xw)
idx <- attr(xa, "idx")
for (j in 1:nrow(xa)) {
  lines(x = idx[j]:(idx[j] + sw_size - 1), y = xa[j, 1:sw_size], col = "green")
}

plot(x = idx_full, y = x, main = "")
lines(x = idx_full, y = x, col = "black")
for (j in 1:nrow(xw)) {
  lines(x = j:(j + sw_size - 1), y = xw[j, 1:sw_size], col = "green")
}
augment <- ts_aug_awaresmooth(0.25)
augment <- fit(augment, xw)
xa <- transform(augment, xw)
idx <- attr(xa, "idx")
for (j in 1:nrow(xa)) {
  lines(x = idx[j]:(idx[j] + sw_size - 1), y = xa[j, 1:sw_size], col = "green")
}

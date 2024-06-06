library(tspredit)
library(daltoolbox)

i <- seq(0, 2*pi+8*pi/50, pi/50)
x <- cos(i)
noise <- rnorm(length(x), 0, sd(x)/10)

x <- x + noise
x[30] <-rnorm(1, 0, sd(x))

x[60] <-rnorm(1, 0, sd(x))

x[90] <-rnorm(1, 0, sd(x))


options(repr.plot.width=6, repr.plot.height=5)  
par(mfrow = c(1, 1))
plot(i, x)
lines(i, x)



sw_size <- 10
xw <- ts_data(x, sw_size)
i <- 1:length(x)

plot(x = i, y = x, main = "")
lines(x = i, y = x, col="black")
for (j in 1:nrow(xw)) {
  lines(x = j:(j+sw_size-1), y = xw[j,1:sw_size], col="green")
}

if (FALSE) {
  augment <- ts_aug_jitter()
  augment <- fit(augment, xw)
  xa <- transform(augment, xw)
  idx <- attr(xa, "idx")
  
  for (j in 1:nrow(xa)) {
    lines(x = idx[j]:(idx[j]+sw_size-1), y = xa[j,1:sw_size], col="green")
  }
}

if (FALSE) {
  augment <- ts_aug_awareness(0.25)
  augment <- fit(augment, xw)
  xa <- transform(augment, xw)
  idx <- attr(xa, "idx")
  
  for (j in 1:nrow(xa)) {
    lines(x = idx[j]:(idx[j]+sw_size-1), y = xa[j,1:sw_size], col="green")
  }
}

if (TRUE) {
  augment <- ts_aug_awaresmooth(0.25)
  augment <- fit(augment, xw)
  xa <- transform(augment, xw)
  idx <- attr(xa, "idx")
  
  for (j in 1:nrow(xa)) {
    lines(x = idx[j]:(idx[j]+sw_size-1), y = xa[j,1:sw_size], col="green")
  }
}


source("header.R")

set.seed(1)
n <- 100  # Number of time points
data <- c(2, 1, 2, 3, 2, 1, 2, 3)
data <- data + 3*(1:length(data))
data <- c(rev(data), 3, data)
x <- 1:length(data)

xb <- c(x[1:8], rep(NA, 9))
datab <- c(data[1:8], rep(NA, 9))

xa <- c(rep(NA, 9), x[10:17])
dataa <- c(rep(NA, 9), data[10:17])

event <- rep(FALSE, n)

model <- fit(harbinger(), data)
detection <- detect(model, data)
detection$event[9] <- TRUE
detection$type[9] <- "changepoint"

grf <- har_plot(model, data, detection)
grf <- grf + ylab(" ")
grf <- grf + font
grf <- grf + geom_smooth(aes(x,data), color = "blue", method=lm, se=FALSE, size = 1, linetype = "dashed")
grf <- grf + annotate(geom="text", x=9, y=16, label="regression for the entire window", color="blue")
grf <- grf + geom_smooth(aes(xb,datab), color = "darkgreen", method=lm, se=FALSE, size = 1, linetype = "dashed")
grf <- grf + annotate(geom="text", x=5.75, y=20, label="regression before change point", color="darkgreen")
grf <- grf + geom_smooth(aes(xa,dataa), color = "red", method=lm, se=FALSE, size = 1, linetype = "dashed")
grf <- grf + annotate(geom="text", x=12.5, y=20, label="regression after change point", color="red")
#plot(grf)

save_png(grf, "figures/chap4_cp.png", 1280, 720)

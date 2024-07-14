source("header.R")
options(scipen=999)
library(ggpmisc)
library(daltoolbox)
library(harbinger)

set.seed(1)
n <- 100  # Number of time points
data <- c(2, 1, 2, 3, 2, 1, 2, 3)
data <- data + 3*(1:length(data))
data <- c(data, 30, rev(data))
x <- 1:length(data)

xb <- c(x[1:9], rep(NA, 8))
datab <- c(data[1:9], rep(NA, 8))

xa <- c(rep(NA, 8), x[9:17])
dataa <- c(rep(NA, 8), data[9:17])

event <- rep(FALSE, n)

model <- fit(harbinger(), data)
detection <- detect(model, data)
detection$event[9] <- TRUE
detection$type[9] <- "changepoint"

grf <- har_plot(model, data, detection)
grf <- grf + ylab(" ")
grf <- grf + font
grf <- grf + geom_smooth(aes(x,data), color = "blue", method=lm, se=FALSE, size = 1, linetype = "dashed")
grf <- grf + annotate(geom="text", x=9, y=15, label="regression for the entire window", color="blue", size=5)
grf <- grf + geom_smooth(aes(xb,datab), color = "darkgreen", method=lm, se=FALSE, size = 1, linetype = "dashed")
grf <- grf + annotate(geom="text", x=3.75, y=25, label="regression before change point", color="darkgreen", size=5)
grf <- grf + geom_smooth(aes(xa,dataa), color = "red", method=lm, se=FALSE, size = 1, linetype = "dashed")
grf <- grf + annotate(geom="text", x=14.5, y=25, label="regression after change point", color="red", size=5)
#plot(grf)

save_png(grf, "figures/chap4_scp_principle.png", 1280, 720)

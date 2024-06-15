source("header.R")

library(daltoolbox)
library(harbinger)

set.seed(1)

data_a <- function() {
  data <- NULL
  data <- c(data, rep(10, 10)) 
  data <- c(data, rev(seq(1, 10, 1)^2/10))
  data <- c(data, rep(0.1, 10)) 
  return(data+15)
}


a <- data_a()
model <- fit(harbinger(), a)
detection <- detect(model, a)
detection$event[c(11,15,21)] <- TRUE
detection$type[c(11,15,21)] <- "anomaly"
event <- rep(FALSE, length(a))
event[c(11,20)] <- TRUE


grf <- har_plot(model, a, detection, event, colors=c("darkgreen", "blue", "red", "purple"))
grf <- grf + ylab(" ") + ylim(0, 30)
grf <- grf + geom_segment(aes(x = 9, y = 2, xend = 11, yend = 6), col="black", linewidth = 0.25, linetype="dashed")
grf <- grf + geom_segment(aes(x = 13, y = 2, xend = 11, yend = 6), col="black", linewidth = 0.25, linetype="dashed")
grf <- grf + geom_segment(aes(x = 11, y = 2, xend = 11, yend = 6), col="black", linewidth = 0.125, linetype="dotted")
grf <- grf + geom_segment(aes(x = 18, y = 2, xend = 20, yend = 6), col="black", linewidth = 0.25, linetype="dashed")
grf <- grf + geom_segment(aes(x = 22, y = 2, xend = 20, yend = 6), col="black", linewidth = 0.25, linetype="dashed")

grf <- grf + geom_segment(aes(x = 1, y = 10, xend = 30, yend = 10), col="black", linewidth = 0.125, linetype="dotted")
grf <- grf + geom_segment(aes(x = 10.9, y = 14, xend = 10.9, yend = 10), col="black", linewidth = 0.125, linetype="dotted")
grf <- grf + geom_segment(aes(x = 11.1, y = 14, xend = 11.1, yend = 10), col="black", linewidth = 0.125, linetype="dotted")
grf <- grf + geom_segment(aes(x = 10.9, y = 14, xend = 11.1, yend = 14), col="black", linewidth = 0.125, linetype="dotted")
grf <- grf + geom_segment(aes(x = 15, y = 10, xend = 15, yend = 9), col="black", linewidth = 0.125, linetype="dotted")
grf <- grf + geom_segment(aes(x = 21, y = 10, xend = 21, yend = 9), col="black", linewidth = 0.125, linetype="dotted")
grf <- grf + annotate(geom="text", x=11, y=15, label="1", color="darkgreen")
grf <- grf + annotate(geom="text", x=15, y=8, label="0", color="red")
grf <- grf + annotate(geom="text", x=21, y=8, label="0", color="red")
grf <- grf + annotate(geom="text", x=2.5, y=11, label="traditional", color="black")

grf <- grf + geom_segment(aes(x = 21, y = 2, xend = 21, yend = 4), col="black", linewidth = 0.125, linetype="dotted")
grf <- grf + geom_segment(aes(x = 15, y = 2, xend = 15, yend = 1), col="black", linewidth = 0.125, linetype="dotted")
grf <- grf + geom_segment(aes(x = 1, y = 2, xend = 30, yend = 2), col="black", linewidth = 0.125, linetype="dotted")
grf <- grf + annotate(geom="text", x=11, y=7, label="1", color="darkgreen")
grf <- grf + annotate(geom="text", x=15, y=0, label="0", color="red")
grf <- grf + annotate(geom="text", x=21.5, y=5.5, label="0.5", color="darkgreen")
grf <- grf + annotate(geom="text", x=2, y=3, label="softed", color="black")

grf <- grf + geom_segment(aes(x = 11, y = 16, xend = 11, yend = 24), col="darkgreen", linewidth = 0.125, linetype="dashed")
grf <- grf + geom_segment(aes(x = 20, y = 10, xend = 20, yend = 14), col="blue", linewidth = 0.125, linetype="dashed")

grf <- grf + geom_segment(aes(x = 11, y = 8, xend = 11, yend = 10), col="darkgreen", linewidth = 0.125, linetype="dashed")
grf <- grf + geom_segment(aes(x = 20, y = 2, xend = 20, yend = 10), col="blue", linewidth = 0.125, linetype="dashed")


grf <- grf + font 

save_png(grf, "figures/chap6_soft_evaluation.png", 1280, 720)




library(RColorBrewer)
library(ggplot2)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

# Project helper: plot theme `font` and utility `save_png()`





# Reproducibility
set.seed(1)
# Simple synthetic series with a peak then plateau
data_a <- function() {
  data <- NULL
  data <- c(data, rep(10, 10))                  # initial level
  data <- c(data, rev((seq(1, 10, 1)^2) / 10))  # descending curve
  data <- c(data, rep(0.1, 10))                 # low tail
  return(data + 5)                               # shift upward for plotting
}
data <- data_a()
# Fit a Harbinger model using the full series
model <- fit(harbinger(), data)

detection <- detect(model, data)

event <- rep(FALSE, length(data))
grfD <- function(data, detection, event) {
  grf <- har_plot(model, data, detection, event)
  grf <- grf + ylab(" ") + ylim(0, 20) + xlim(0, 30)
  grf <- grf + font
  grf <- grf + ggplot2::annotate(geom="text", x=11, y=16, label="E[1]", color="red", parse=TRUE)
  grf <- grf + ggplot2::annotate(geom="text", x=20, y=6, label="E[2]", color="red", parse=TRUE)
  grf <- grf + ggplot2::annotate(geom="text", x=12, y=14, label="B", color="blue", parse=TRUE)
  grf <- grf + ggplot2::annotate(geom="text", x=21, y=6, label="B", color="blue", parse=TRUE)
  grf <- grf + ggplot2::annotate(geom="text", x=10, y=16, label="A", color="blue", parse=TRUE)
  grf <- grf + geom_point(aes(11, 15), colour = "red", size = 1)  
  grf <- grf + geom_point(aes(20, 5), colour = "red", size = 1)  
  grf <- grf + geom_point(aes(10, 15), colour = "blue", size = 1.25)  
  grf <- grf + geom_point(aes(21, 5), colour = "blue", size = 1.25)  
  grf <- grf + geom_point(aes(12, data[12]), colour = "blue", size = 1.25)  
}
grfd <- grfD(data, detection, event)

#save_png(grfd, "figures/chap7_tolerance.png", 1280, 720)
grfd

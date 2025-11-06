library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

data(examples_changepoints)  # built-in dataset from Harbinger examples

data <- examples_changepoints$complex        # select the complex example series
model <- fit(harbinger(), data$serie)        # fit default detector
detection <- detect(model, data$serie)       # compute detections (events/scores)

# Base plot with detections overlaid
grf <- har_plot(model, data$serie, detection)
# Cosmetic improvements and annotations
grf <- grf + scale_x_continuous(
  breaks = seq(0, length(data$serie), by = length(data$serie)/5),
  name = "example"
)
grf <- grf + ggplot2::annotate(geom = "text", x = 50, y = 12.5, label = "A", color = "black")
grf <- grf + geom_vline(xintercept = 100, col = "darkgray", linewidth = 0.5, linetype = "dashed")
grf <- grf + ggplot2::annotate(geom = "text", x = 111, y = 11.8, label = "cp[AB]", color = "black", parse = TRUE)
grf <- grf + ggplot2::annotate(geom = "text", x = 150, y = 12.5, label = "B", color = "black")
grf <- grf + geom_vline(xintercept = 200, col = "darkgray", linewidth = 0.5, linetype = "dashed")
grf <- grf + ggplot2::annotate(geom = "text", x = 211, y = 11.8, label = "cp[BC]", color = "black", parse = TRUE)
grf <- grf + ggplot2::annotate(geom = "text", x = 250, y = 12.5, label = "C", color = "black")
grf <- grf + geom_vline(xintercept = 300, col = "darkgray", linewidth = 0.5, linetype = "dashed")
grf <- grf + ggplot2::annotate(geom = "text", x = 311, y = 11.8, label = "cp[CD]", color = "black", parse = TRUE)
grf <- grf + ggplot2::annotate(geom = "text", x = 350, y = 12.5, label = "D", color = "black")
grf <- grf + geom_vline(xintercept = 400, col = "darkgray", linewidth = 0.5, linetype = "dashed")
grf <- grf + ggplot2::annotate(geom = "text", x = 411, y = 11.8, label = "cp[DE]", color = "black", parse = TRUE)
grf <- grf + ggplot2::annotate(geom = "text", x = 450, y = 12.5, label = "E", color = "black")
grf <- grf + ylab("value") + font
# Save figure
#save_png(grf, "figures/chap4_example.png", 1280, 720)
grf

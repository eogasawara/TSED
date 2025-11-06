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

data("examples_anomalies")                      # built-in toy anomalous series
dataset <- examples_anomalies$tt_warped         # choose time-warped example
dataset$event <- factor(dataset$event, labels = c("FALSE", "TRUE"))
head(dataset)

# Quick look at the raw series
plot_ts(x = seq_along(dataset$serie), y = dataset$serie)
# Keep label levels for later use (optional)
slevels <- levels(dataset$event)
# Temporal split: first 75 points for training, the rest for testing
train <- dataset[1:75, ]
test  <- dataset[-(1:75), ]

model <- harbinger()

model <- fit(model, train$serie)                     # fit on training window
detection <- detect(model, dataset$serie)            # detect over full series
grf <- har_plot(model, dataset$serie, detection, as.logical(dataset$event))
grf <- grf + geom_vline(xintercept = 75, col = "black", linetype = "dashed")
grf <- grf + font

#save_png(grf, "figures/chap3_example.png", 1280, 720)
grf

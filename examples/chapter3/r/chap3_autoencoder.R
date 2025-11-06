library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")



data("examples_anomalies")
dataset <- examples_anomalies$tt_warped
dataset$event <- factor(dataset$event, labels = c("FALSE", "TRUE"))
# Visual check and simple train/test split
plot_ts(x = seq_along(dataset$serie), y = dataset$serie)
train <- dataset[1:75, ]

model <- han_autoencoder(3, 1)
model <- fit(model, dataset$serie)

detection <- detect(model, dataset$serie)

grf <- har_plot(model, dataset$serie, detection, as.logical(dataset$event))

grf <- grf + geom_vline(xintercept = 75, col = "black", linetype = "dashed")
grf <- grf + font

#save_png(grf, "figures/chap3_autoencoder.png", 1280, 720)
grf

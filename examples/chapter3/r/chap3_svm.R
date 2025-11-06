library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")



data("examples_anomalies")
dataset <- examples_anomalies$tt_warped
dataset$event <- factor(dataset$event, labels = c("FALSE", "TRUE"))
plot_ts(x = seq_along(dataset$serie), y = dataset$serie)
train <- dataset[1:75, ]

slevels <- levels(dataset$event)
train_n <- train; dataset_n <- dataset
model <- hanc_ml(cla_svm("event", slevels, epsilon = 0.0, cost = 20.000))
model <- fit(model, train_n)

detection <- detect(model, dataset_n)

grf <- har_plot(model, dataset_n$serie, detection, as.logical(dataset_n$event))

grf <- grf + geom_vline(xintercept = 75, col = "black", linetype = "dashed") + font

#save_png(grf, "figures/chap3_svm.png", 1280, 720)
grf

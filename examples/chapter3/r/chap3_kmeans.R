library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")



data("examples_anomalies")
dataset <- examples_anomalies$tt_warped

plot_ts(x = seq_along(dataset$serie), y = dataset$serie)
model <- hanct_kmeans(1)
model <- fit(model, dataset$serie)

detection <- detect(model, dataset$serie)

print(detection |> dplyr::filter(event == TRUE))

evaluation <- daltoolbox::evaluate(model, detection$event, dataset$event)

print(evaluation$confMatrix)

grf <- har_plot(model, dataset$serie, detection, dataset$event)

grf <- grf + font

#save_png(grf, "figures/chap3_kmeans.png", 1280, 720)
grf

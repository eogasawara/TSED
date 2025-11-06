library(ggplot2)
library(RColorBrewer)
library(ggpmisc)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

options(scipen = 999)

data("examples_anomalies")
# Use the time-warped example with labels
dataset <- examples_anomalies$tt_warped
dataset$event <- factor(dataset$event, labels = c("FALSE", "TRUE"))
# Visualize raw series
plot_ts(x = seq_along(dataset$serie), y = dataset$serie)
# Temporal split: first 75 points as train
train <- dataset[1:75, ]
test  <- dataset[-(1:75), ]

model <- hanr_arima()

# Fit on training subset
model <- fit(model, train$serie)

# Detect over the full sequence
detection <- detect(model, dataset$serie)

# Optional: build in-sample fitted values using daltoolbox for plotting
ts <- ts_data(dataset$serie, 0)
io <- ts_projection(ts)
model_ts <- ts_arima()
model_ts <- fit(model_ts, x = io$input, y = io$output)
adjust <- as.vector(predict(model_ts, io$input))

grf <- har_plot(model, dataset$serie, detection, as.logical(dataset$event))

grf <- grf + geom_vline(xintercept = 75, col = "black", linetype = "dashed")
grf <- grf + geom_line(aes(y = adjust), linetype = "dashed", col = "darkblue")
grf <- grf + geom_point(aes(y = adjust), size = 0.25, col = "darkblue")
grf <- grf + font

#save_png(grf, "figures/chap3_arima.png", 1280, 720)
grf

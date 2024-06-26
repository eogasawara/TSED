source("header.R")
library(daltoolbox)
library(harbinger)

# loading example
data(examples_changepoints)
data <- examples_changepoints$simple

model <- fit(hcp_cf_lr(sw_size = 10), data$serie)
detection <- detect(model, data$serie)
print(detection$idx[detection$event])

grf <- har_plot(model, data$serie, detection)
grf <- grf + ylab("value")
grf <- grf + font

save_png(grf, "backup/chap4_cf_arima.png", 1280, 720)

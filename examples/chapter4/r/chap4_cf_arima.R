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

data(examples_changepoints)

data <- examples_changepoints$simple
model <- fit(hcp_cf_lr(sw_size = 10), data$serie)  # sliding-window size for residual change
detection <- detect(model, data$serie)
print(detection$idx[detection$event])

grf <- har_plot(model, data$serie, detection) + ylab("value") + font
#save_png(grf, "figures/chap4_cf_arima.png", 1280, 720)
grf

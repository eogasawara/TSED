library(RColorBrewer)
library(ggplot2)
library(daltoolbox)
library(harbinger)
library(gridExtra)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

library(RColorBrewer)
library(ggplot2)
library(daltoolbox)
library(harbinger)
library(gridExtra)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

har_outliers_checks_highgroup <- function(outliers, values) {
  threshold <- attr(outliers, "threshold")
  values <- abs(values)
  if (is_matrix_or_df(values)) values <- rowSums(values)
  size <- length(values)
  group <- split(outliers, cumsum(c(1, diff(outliers) != 1)))
  keep <- rep(FALSE, size)
  for (g in group) {
    if (length(g) > 0) {
      i <- which.max(values[g]); i <- g[i]
      keep[i] <- TRUE
    }
  }
  attr(keep, "threshold") <- threshold
  return(keep)
}

data(examples_anomalies)

dataset <- examples_anomalies$simple

grf1 <- har_plot(harbinger(), dataset$serie)

model <- hanr_arima()

model <- fit(model, dataset$serie)

detection <- detect(model, dataset$serie)

grf2 <- har_plot(model, attr(detection, "res"), detection,
                 dataset$event, yline = attr(detection, "threshold"))

model <- hanr_arima()

model$har_outliers <- harutils()$har_outliers_boxplot
model <- fit(model, dataset$serie)

detection <- detect(model, dataset$serie)

grf3 <- har_plot(model, attr(detection, "res"), detection,
                 dataset$event, yline = attr(detection, "threshold"))

model <- hanr_arima()

model$har_outliers_check <- harutils()$har_outliers_checks_highgroup  
model <- fit(model, dataset$serie)

detection <- detect(model, dataset$serie)

grf4 <- har_plot(model, attr(detection, "res"), detection,
                 dataset$event, yline = attr(detection, "threshold"))

# mypng(file = "threshold.png", width = 1440, height = 1260)
gridExtra::grid.arrange(grf1, grf2, grf3, grf4,
                        layout_matrix = matrix(c(1, 2, 3, 4), byrow = TRUE, ncol = 2))
# dev.off()

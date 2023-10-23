source("header.R")

source("header.R")
library(readxl)
library(daltoolbox)
library(dplyr)
library(stats)

# Load necessary libraries

n <- 78  # Number of time points
data <- c(sin((0:n)/pi), 2*sin((0:19)/pi), sin((0:n)/pi))
event <- rep(FALSE, n)

model <- hanr_garch()
model <- fit(model, data)
detection <- detect(model, data)
print(detection |> dplyr::filter(event==TRUE))
print(nrow(detection |> dplyr::filter(event==TRUE)))
grf <- har_plot(model, data, detection) +
  fontstyle + font + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
grf <- grf + geom_vline(xintercept = 79, col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + geom_vline(xintercept = 99, col="darkgray", linewidth = 0.5, linetype="dashed")
save_png(grf, "figures/chap3_garch.png", 1280, 720)


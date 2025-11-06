library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(forecast)
library(daltoolbox)
library(daltoolboxdp)
library(tspredit)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(forecast)
library(daltoolbox)
library(daltoolboxdp)
library(tspredit)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

data(examples_harbinger)

dataset <- examples_harbinger$nonstationarity

har_plot(harbinger(), dataset$serie)

model_a <- hanr_fbiad()

model_a <- fit(model_a, dataset$serie)

detection_a <- detect(model_a, dataset$serie)

print(detection_a |> dplyr::filter(event == TRUE))

grfa <- har_plot(model_a, dataset$serie, detection_a, dataset$event) + 
  labs(caption = "(a) FBIAD") + theme(plot.caption = element_text(hjust = 0.5)) + font

model_b <- hanr_ml(ts_lstm(ts_norm_diff(), input_size = 4, epochs = 10000))

model_b <- fit(model_b, dataset$serie)

detection_b <- detect(model_b, dataset$serie)

print(detection_b |> dplyr::filter(event == TRUE))

grfb <- har_plot(model_b, dataset$serie, detection_b, dataset$event) + 
  labs(caption = "(b) LSTM") + theme(plot.caption = element_text(hjust = 0.5)) + font

model_c <- hcp_gft()

model_c <- fit(model_c, dataset$serie)

detection_c <- detect(model_c, dataset$serie)

print(detection_c |> dplyr::filter(event == TRUE))

grfc <- har_plot(model_c, dataset$serie, detection_c, dataset$event) + 
  labs(caption = "(c) GFT") + theme(plot.caption = element_text(hjust = 0.5)) + font

detection_d <- detection_c
detection_d$event <- detection_a$event | detection_b$event | detection_c$event
detection_d$type[(!detection_c$event)] <- "anomaly"
detection_d$type[!detection_d$event] <- ""

grfd <- har_plot(harbinger(), dataset$serie, detection_d, dataset$event) + 
  labs(caption = "(d) Integrated View (FBIAD+LSTM+GFT)") + theme(plot.caption = element_text(hjust = 0.5)) + font

# mypng(file = "figures/multiple-detection.png", width = 1600, height = 1260)
gridExtra::grid.arrange(grfa, grfb, grfc, grfd,
                        layout_matrix = matrix(c(1, 2, 3, 4), byrow = TRUE, ncol = 2))
# dev.off()

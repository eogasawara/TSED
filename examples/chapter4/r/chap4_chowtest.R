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
data <- examples_changepoints$complex

model <- fit(hcp_chow(), data$serie)
detection <- detect(model, data$serie)
print(detection$idx[detection$event])  # indices flagged by the test

grf <- har_plot(model, data$serie, detection)
grf <- grf + ylab("value") + font
#save_png(grf, "figures/chap4_chowtest.png", 1280, 720)
grf

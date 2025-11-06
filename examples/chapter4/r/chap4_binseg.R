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

model <- fit(hcp_binseg(Q = 10), data$serie)  # Q: max number of changes
detection <- detect(model, data$serie)
print(detection$idx[detection$event])

grf <- har_plot(model, data$serie, detection) + ylab("value") + font
#save_png(grf, "figures/chap4_binseg.png", 1280, 720)
grf

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

data(examples_motifs)
data <- examples_motifs$mitdb102
rownames(data) <- 1:nrow(data)
data$event <- FALSE

# hmo_mp parameters:
# - mode = "stamp": randomized MP algorithm
# - w = 25: window size
# - qtd = 10: number of motifs to return
model <- fit(hmo_mp(mode = "stamp", w = 25, qtd = 10), data$serie)
detection <- detect(model, data$serie)
print(detection[detection$event, ])

grf <- har_plot(model, data$serie, detection) + font
#save_png(grf, "figures/chap5_motifs_mp.png", 1280, 720)
grf

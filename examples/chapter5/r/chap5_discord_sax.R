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
# hdis_sax parameters (typical):
# - alphabet size (e.g., 26 symbols)
# - window size for subsequences (e.g., 25)
model <- hdis_sax(26, 25)
model <- fit(model, data$serie)
detection <- detect(model, data$serie)
print(detection[detection$event, ])

grf <- har_plot(model, data$serie, detection) + font
#save_png(grf, "figures/chap5_discords_sax.png", 1280, 720)
grf

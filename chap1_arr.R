source("header.R")
library(daltoolbox)
library(harbinger)


#loading the example database
load(url("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/data/examples_motifs.RData"))
data <- examples_motifs$mitdb102
rownames(data) <- 1:nrow(data)
data$event <- FALSE

model <- fit(harbinger(), data$serie)
detection <- detect(model, data$serie)

grf <- har_plot(model, data$serie, detection, data$event) +
  font + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
save_png(grf, "figures/chap1_arr.png", 1280, 720)

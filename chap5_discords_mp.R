source("header.R")
library(daltoolbox)
library(harbinger)

#loading the example database
data(examples_motifs)
data <- examples_motifs$mitdb102
rownames(data) <- 1:nrow(data)
data$event <- FALSE

model <- fit(hdis_mp(mode = "stamp", w = 25, qtd = 10), data$serie)
detection <- detect(model, data$serie)
print(detection[detection$event,])

grf <- har_plot(model, data$serie, detection)
grf <- grf + font

save_png(grf, "figures/chap5_discords_mp.png", 1280, 720)


source("header.R")
library(daltoolbox)
library(harbinger)

source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/R/hdis_sax.R")
source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/R/har_plot.R")

#loading the example database
load(url("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/data/examples_motifs.RData"))
data <- examples_motifs$mitdb102
rownames(data) <- 1:nrow(data)
data$event <- FALSE

model <- hdis_sax(26, 25)
model <- fit(model, data$serie)
detection <- detect(model, data$serie)
print(detection[detection$event,])

grf <- har_plot(model, data$serie, detection)
grf <- grf + font

save_png(grf, "figures/chap5_discords_sax.png", 1280, 720)


source("header.R")
library(daltoolbox)
library(harbinger)
source("hmo_discord.R")
source("har_plot.R")

#loading the example database
load(url("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/data/examples_motifs.RData"))
data <- examples_motifs$mitdb102
rownames(data) <- 1:nrow(data)
data$event <- FALSE

model <- fit(hmo_discord(mode = "pmp", w = 200, qtd = 10), data$serie)
detection <- detect(model, data$serie)
print(detection[detection$event,])
detection$seq[6218] <- 2

grf <- har_plot(model, data$serie, detection)
grf <- grf + font

save_png(grf, "figures/chap5_discords_mp.png", 1280, 720)


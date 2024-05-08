source("header.R")
library(daltoolbox)
library(harbinger)
source("hmo_mp.R")
source("har_plot.R")

#loading the example database
load(url("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/data/examples_motifs.RData"))
data <- examples_motifs$mitdb102
rownames(data) <- 1:nrow(data)
data$event <- FALSE

model <- fit(hmo_mp(mode = "stomp", w = 25, qtd = 10), data$serie)
detection <- detect(model, data$serie)
#detection$event[detection$seq > 1] <- FALSE
print(detection[detection$event,])

grf <- har_plot(model, data$serie, detection)
grf <- grf + font
plot(grf)
save_png(grf, "figures/chap5_motifs_mp.png", 1280, 720)


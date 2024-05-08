source("header.R")
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/R/hmo_mp.R")
source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/R/har_plot.R")

#loading the example database
load(url("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/data/examples_motifs.RData"))
data <- examples_motifs$mitdb102
rownames(data) <- 1:nrow(data)

#data$event <- FALSE

model <- fit(harbinger(), data$serie)
detection <- detect(model, data$serie)
detection$type <- NA
detection$seq <- NA
detection$seqlen <- NA
detection$event[data$event] <- TRUE
detection$type[data$event] <- "motif"
detection$seq[data$event] <- 1
detection$seqlen[data$event] <- 50
print(detection[detection$event,])

grf <- har_plot(model, data$serie, detection, data$event) +
  font + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
save_png(grf, "figures/chap1_labeled_motif.png", 1280, 720)

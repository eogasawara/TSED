source("header.R")
library(daltoolbox)
library(harbinger)

# loading example
load(url("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/data/examples_changepoints.RData"))
data <- examples_changepoints$complex

model <- fit(hcp_scp(sw_size = 30), data$serie)
detection <- detect(model, data$serie)
print(detection$idx[detection$event])

grf <- har_plot(model, data$serie, detection)
grf <- grf + ylab("value")
grf <- grf + font

save_png(grf, "figures/chap4_scp.png", 1280, 720)

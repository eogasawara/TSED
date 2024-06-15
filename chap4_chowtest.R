source("header.R")
library(daltoolbox)
library(harbinger)

# loading example
data(examples_changepoints)
data <- examples_changepoints$complex

model <- fit(hcp_chow(), data$serie)
detection <- detect(model, data$serie)
print(detection$idx[detection$event])

grf <- har_plot(model, data$serie, detection)
grf <- grf + ylab("value")
grf <- grf + font

save_png(grf, "figures/chap4_chow.png", 1280, 720)

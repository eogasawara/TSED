source("header.R")
library(daltoolbox)
library(harbinger)

# loading example
data(examples_changepoints)
data <- examples_changepoints$complex

model <- fit(hcp_binseg(Q = 10), data$serie)
detection <- detect(model, data$serie)
print(detection$idx[detection$event])

grf <- har_plot(model, data$serie, detection)
grf <- grf + ylab("value")
grf <- grf + font

save_png(grf, "backup/chap4_binseg.png", 1280, 720)

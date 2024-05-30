source("header.R")
library(daltoolbox)
library(harbinger)

source(url("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/develop/hcd_page_hinkley.R"))

data(examples_changepoints)
data <- examples_changepoints$complex
data$event <- NULL

model <- fit(hcd_page_hinkley(threshold=3), data)
detection <- detect(model, data)
print(detection[(detection$event),])

grf <- har_plot(model, data$serie, detection)
grf <- grf + ylab("value")
grf <- grf + font


save_png(grf, "figures/chap4_page_hinkley.png", 1280, 720)

source("header.R")
library(daltoolbox)
library(harbinger)

set.seed(1)
n <- 100  # Number of time points
data <- c(sin((1:n)/pi), 2*sin((1:n)/pi), 10 + sin((1:n)/pi), 10-10/n*(1:n)+sin((1:n)/pi)/2, sin((1:n)/pi)/2)
event <- rep(FALSE, n)

model <- fit(hcp_gft(), data)
detection <- detect(model, data)
print(detection$idx[detection$event])

grf <- har_plot(model, data, detection)
grf <- grf + ylab("value")
grf <- grf + font


save_png(grf, "figures/chap4_gft.png", 1280, 720)


source("header.R")
library(readxl)
library(daltoolbox)
library(dplyr)
library(stats)

set.seed(1)
n <- 200
data <- c(rnorm(n/2, mean = 0, sd = 1), rnorm(n/2, mean = 3, sd = 1))

model <- fit(hcp_cf_arima(sw_size = 30), data)
detection <- detect(model, data)

grfBasic <- har_plot(model, data, detection)
grfBasic <- grfBasic + ylab("value")
grfBasic <- grfBasic + font

n <- 100  # Number of time points
data <- c(sin((1:n)/pi), 2*sin((1:n)/pi), 10 + sin((1:n)/pi), 10-10/n*(1:n)+sin((1:n)/pi)/2, sin((1:n)/pi)/2)
event <- rep(FALSE, n)

model <- fit(hcp_cf_arima(sw_size = 30), data)
detection <- detect(model, data)

grf <- har_plot(model, data, detection)
grf <- grf + ylab("value")
grf <- grf + font

save_png(grf, "figures/chap4_cf_arima.png", 1280, 720)

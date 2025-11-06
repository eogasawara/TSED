library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
library(heimdall)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
library(heimdall)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

data(examples_changepoints)
data <- examples_changepoints$complex
data$event <- NULL
data$prediction <- data$serie > 4                   # simple threshold-based prediction
model <- dfr_ddm()
detection <- NULL
state <- list(obj = model, pred = FALSE)
for (i in seq_along(data$prediction)) {
  state <- update_state(state$obj, data$prediction[i])  # online update with prediction correctness
  if (state$drift) {
    type <- 'changepoint'
    state$obj <- reset_state(state$obj)                 # reset after drift
  } else {
    type <- ''
  }
  detection <- rbind(detection, data.frame(idx = i, event = state$drift, type = type))
}

grf <- har_plot(model, data$serie, detection)
grf <- grf + ylab("value")
#save_png(grf, "figures/chap4_ddm.png", 1280, 720)
grf

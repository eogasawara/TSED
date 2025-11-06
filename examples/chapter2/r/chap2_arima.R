library(ggplot2)
library(RColorBrewer)
library(ggpmisc)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

options(scipen = 999)

data(examples_harbinger)  # built-in examples
data <- examples_harbinger$global_temperature_yearly  # yearly temperature series
data$event <- FALSE                                   # no explicit event labels here
# Convert to internal ts format; horizon 0 (one-step ahead)
ts <- ts_data(data$serie, 0)
# Hold out the last 4 observations for testing
test_size <- 4
samp <- ts_sample(ts, test_size)
ts_head(samp$train, 3)                                 # quick peek at training structure
# Define ARIMA model and fit on training projections
model <- ts_arima()
io_train <- ts_projection(samp$train)
model <- daltoolbox::fit(model, x = io_train$input, y = io_train$output)
# In-sample fitted values for diagnostics
adjust <- predict(model, io_train$input)

ev_adjust <- daltoolbox::evaluate(model, io_train$output, adjust)  # training metrics
print(head(ev_adjust$metrics))                         # show a few metrics
# Multi-step-ahead prediction over the held-out window
steps_ahead <- 4
io_test <- ts_projection(samp$test)
prediction <- predict(model, x = io_test$input, steps_ahead = steps_ahead)
prediction <- as.vector(prediction)
output <- as.vector(io_test$output)
if (steps_ahead > 1) {
  output <- output[1:steps_ahead]
}
# Quick glance at observed vs predicted
print(sprintf("%.2f, %.2f", output, prediction))

ev_test <- daltoolbox::evaluate(model, output, prediction)  # test metrics
print(head(ev_test$metrics))
# Prepare time-series objects for plotting
yvalues <- c(io_train$output, io_test$output)
params <- attr(model, "params")                  # ARIMA(p,d,q) for captioning
temperature <- data$serie[-c(1:100)]              # align with plotting range used
adjust <- adjust[-c(1:100)]
yts <- ts(temperature, frequency = 1, start = c(1950, 1))
yhat <- ts(c(adjust, prediction), frequency = 1, start = c(1950, 1))
# Split line series for fitted vs. forecasted segments
yhatadj <- yhat
yhatadj[(length(adjust) + 1):length(yhat)] <- NA
yhatpred <- yhat
yhatpred[1:(length(adjust) - 1)] <- NA

grf <- autoplot(yts, col = "black")
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature") + xlab("time")
grf <- grf + geom_point(aes(y = yts), size = 0.5, col = "black")
grf <- grf + geom_line(aes(y = yhatadj), col = "darkblue", linetype = "dashed")
grf <- grf + geom_point(aes(y = yhatadj), size = 0.5, col = "darkblue")
grf <- grf + geom_line(aes(y = yhatpred), col = "red", linetype = "dashed")
grf <- grf + geom_point(aes(y = yhatpred), size = 0.5, col = "red")
grf <- grf + labs(caption = sprintf("(a) ARIMA(%d, %d, %d) four-step-ahead prediction", params$p, params$d, params$q))
grf <- grf + theme(plot.caption = element_text(hjust = 0.5)) + font
grfA <- grf

#save_png(grfA, "figures/chap2_arima.png", 1280, 720)
grfA

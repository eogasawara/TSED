---
title: "Chapter 2: Arima"
output: html_document
---

``` r
library(ggplot2)
library(RColorBrewer)
library(ggpmisc)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")
```
## Theoretical Overview
This example focuses on ARIMA-based modeling and residual detection. Autoregressive Integrated Moving Average (ARIMA) models capture autocorrelation and trend components; events are indicated by unusual residuals or structural breaks.
## Example Overview and Goals
We demonstrate a complete, reproducible workflow: setting up libraries, loading data, configuring a detector, fitting it, running detection, optionally evaluating, and visualizing the results.
### Knitr Options
Keep chunks tidy and reproducible.

### Setup and Libraries
Load project helpers and set R options to avoid scientific notation where not needed.

``` r
options(scipen = 999)
```
### Data Loading and Prep
Read the dataset and perform minimal preparation (define train/test windows).

``` r
data(examples_harbinger)  # built-in examples
```

``` r
data <- examples_harbinger$global_temperature_yearly  # yearly temperature series
data$event <- FALSE                                   # no explicit event labels here
# Convert to internal ts format; horizon 0 (one-step ahead)
ts <- ts_data(data$serie, 0)
# Hold out the last 4 observations for testing
test_size <- 4
samp <- ts_sample(ts, test_size)
ts_head(samp$train, 3)                                 # quick peek at training structure
```

```
##            t0
## [1,] 13.72417
## [2,] 13.80667
## [3,] 13.79417
```

``` r
# Define ARIMA model and fit on training projections
model <- ts_arima()
io_train <- ts_projection(samp$train)
model <- daltoolbox::fit(model, x = io_train$input, y = io_train$output)
# In-sample fitted values for diagnostics
adjust <- predict(model, io_train$input)
```
### Evaluation
Compute evaluation metrics or diagnostics if ground truth is available.

``` r
ev_adjust <- daltoolbox::evaluate(model, io_train$output, adjust)  # training metrics
```

``` r
print(head(ev_adjust$metrics))                         # show a few metrics
```

```
##           mse       smape        R2
## 1 0.008276672 0.005299228 0.9186967
```

``` r
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
```

```
## [1] "14.88, 14.77" "14.91, 14.79" "14.76, 14.79" "14.81, 14.80"
```
### Evaluation
Compute evaluation metrics or diagnostics if ground truth is available.

``` r
ev_test <- daltoolbox::evaluate(model, output, prediction)  # test metrics
```

``` r
print(head(ev_test$metrics))
```

```
##           mse       smape        R2
## 1 0.007152251 0.004637494 -1.054839
```

``` r
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
```
### Visualization and Output
Plot the series with detected events and optionally save figures. Combine ggplot layers in one chunk for clarity.

``` r
grf <- autoplot(yts, col = "black")
```

``` r
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
```
### Visualization and Output
Plot the series with detected events and optionally save figures. Combine ggplot layers in one chunk for clarity.

``` r
#save_png(grfA, "figures/chap2_arima.png", 1280, 720)
grfA
```

![plot of chunk save-fig](fig/chap2_arima/save-fig-1.png)
## References
* Box, G. E. P., Jenkins, G. M., Reinsel, G. C., & Ljung, G. M. (2015). Time Series Analysis: Forecasting and Control.
* Ogasawara, E., Salles, R., Porto, F., Pacitti, E. Event Detection in Time Series. Springer, 2025. doi:10.1007/978-3-031-75941-3.

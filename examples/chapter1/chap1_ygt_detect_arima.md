---
title: "Chapter 1: Ygt Detect Arima"
output: html_document
---

``` r
library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")
```
## Theoretical Overview
This example focuses on ARIMA-based modeling and residual detection. Autoregressive Integrated Moving Average (ARIMA) models capture autocorrelation and trend; events are indicated by unusual residuals or structural breaks.
## Example Overview and Goals
We will: set up libraries, load data, configure and fit an ARIMA-based detector, run detection, inspect results, and visualize the output.
### What You Will Do
You will: prepare the environment, load a dataset, configure an ARIMA-based detector, fit it, detect events, inspect detections, and visualize the results.
### Setup and Libraries
Load project utilities and required packages.

``` r
# Shared helpers (themes, saving utilities, etc.)
```
### Data Loading and Prep
Read the dataset and prepare it for modeling.

``` r
# Load example time series
data(examples_harbinger)
# Select yearly global temperature series and initialize labels
data <- examples_harbinger$global_temperature_yearly
data$event <- FALSE
```
### Model Configuration
Define the ARIMA-based detector and key options.

``` r
# ARIMA-based anomaly/residual detector
model <- hanr_arima()
```
### Fit the Model
Train (fit) the detector to the time series.

``` r
model <- fit(model, data$serie)
```
### Event Detection
Run the detector to produce event flags and scores.

``` r
detection <- detect(model, data$serie)
```
### Inspect Results
Display detected events, if any, for a quick check.

``` r
print(detection |> dplyr::filter(event == TRUE))
```

```
##   idx event    type
## 1  28  TRUE anomaly
## 2 115  TRUE anomaly
## 3 141  TRUE anomaly
```
### Visualization and Output
Plot the series with detected events and save the figure.

``` r
grf <- har_plot(model, data$serie, detection, data$event, idx = data$i) +
  font +
  scale_x_date(
    breaks = "10 years",
    date_labels = "%Y",
    limits = c(as.Date("1850-01-01"), as.Date("2030-01-01"))
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
#save_png(grf, "figures/chap1_ygt_detect_arima.png", 1280, 720)
grf
```

![plot of chunk visualize](fig/chap1_ygt_detect_arima/visualize-1.png)
## References
* Box, G. E. P., Jenkins, G. M., Reinsel, G. C., & Ljung, G. M. (2015). Time Series Analysis: Forecasting and Control.
* Ogasawara, E., Salles, R., Porto, F., Pacitti, E. Event Detection in Time Series. Springer, 2025. doi:10.1007/978-3-031-75941-3.

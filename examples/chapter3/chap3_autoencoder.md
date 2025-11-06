---
title: "Chapter 3: Autoencoder"
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
This example uses an autoencoder-based detector to reconstruct the series and flag anomalies.
## Example Overview and Goals
We will: load a labeled anomalies dataset, configure a simple autoencoder, fit, detect, and visualize results.
### What You Will Do
Prepare the environment, fit an autoencoder, run detection, and plot with a train/test marker.
### Setup and Libraries
Load helpers and packages.

### Data Loading and Prep
Load a toy anomalies dataset.

``` r
data("examples_anomalies")
dataset <- examples_anomalies$tt_warped
dataset$event <- factor(dataset$event, labels = c("FALSE", "TRUE"))
# Visual check and simple train/test split
plot_ts(x = seq_along(dataset$serie), y = dataset$serie)
```

![plot of chunk data_loading](fig/chap3_autoencoder/data_loading-1.png)

``` r
train <- dataset[1:75, ]
```
### Other Steps
Additional supporting steps that glue the workflow.

``` r
model <- han_autoencoder(3, 1)
model <- fit(model, dataset$serie)
```
### Event Detection
Run the detector to obtain event flags or scores.

``` r
detection <- detect(model, dataset$serie)
```
### Other Steps
Additional supporting steps that glue the workflow.
### Visualization and Output
Plot the series with detected events and optionally save figures. Combine ggplot layers in one chunk for clarity.

``` r
grf <- har_plot(model, dataset$serie, detection, as.logical(dataset$event))
```
### Other Steps
Additional supporting steps that glue the workflow.

``` r
grf <- grf + geom_vline(xintercept = 75, col = "black", linetype = "dashed")
grf <- grf + font
```
### Visualization and Output
Plot the series with detected events and optionally save figures. Combine ggplot layers in one chunk for clarity.

``` r
#save_png(grf, "figures/chap3_autoencoder.png", 1280, 720)
grf
```

![plot of chunk viz_save](fig/chap3_autoencoder/viz_save-1.png)
## References
* General: Box, G. E. P. & Jenkins, G. (1970). Time Series Analysis.
* Ogasawara, E., Salles, R., Porto, F., Pacitti, E. Event Detection in Time Series. Springer, 2025. doi:10.1007/978-3-031-75941-3.

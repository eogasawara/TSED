---
title: "Chapter 3: Svm"
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
This example uses Support Vector Machines (SVM) to classify windowed time-series features, where the margin separates normal from anomalous behavior.
## Example Overview and Goals
We will: load a labeled dataset, configure an SVM classifier, fit on the train split, detect over the full series, and visualize results.
### What You Will Do
Prepare the environment, train an SVM classifier, detect, and plot detections.
### Setup and Libraries
Load helpers and packages.

### Data Loading and Prep
Load labeled anomalies and create a train/test split.

``` r
data("examples_anomalies")
dataset <- examples_anomalies$tt_warped
dataset$event <- factor(dataset$event, labels = c("FALSE", "TRUE"))
plot_ts(x = seq_along(dataset$serie), y = dataset$serie)
```

![plot of chunk data_loading](fig/chap3_svm/data_loading-1.png)

``` r
train <- dataset[1:75, ]
```
### Other Steps
Additional supporting steps that glue the workflow.

``` r
slevels <- levels(dataset$event)
train_n <- train; dataset_n <- dataset
model <- hanc_ml(cla_svm("event", slevels, epsilon = 0.0, cost = 20.000))
model <- fit(model, train_n)
```
### Event Detection
Run the detector to obtain event flags or scores.

``` r
detection <- detect(model, dataset_n)
```
### Other Steps
Additional supporting steps that glue the workflow.
### Visualization and Output
Plot the series with detected events and optionally save figures. Combine ggplot layers in one chunk for clarity.

``` r
grf <- har_plot(model, dataset_n$serie, detection, as.logical(dataset_n$event))
```
### Other Steps
Additional supporting steps that glue the workflow.

``` r
grf <- grf + geom_vline(xintercept = 75, col = "black", linetype = "dashed") + font
```
### Visualization and Output
Plot the series with detected events and optionally save figures. Combine ggplot layers in one chunk for clarity.

``` r
#save_png(grf, "figures/chap3_svm.png", 1280, 720)
grf
```

![plot of chunk viz_save](fig/chap3_svm/viz_save-1.png)
## References
* Cortes, C., & Vapnik, V. (1995). Support-vector networks.
* Ogasawara, E., Salles, R., Porto, F., Pacitti, E. Event Detection in Time Series. Springer, 2025. doi:10.1007/978-3-031-75941-3.

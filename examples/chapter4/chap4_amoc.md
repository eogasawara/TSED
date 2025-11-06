---
title: "Chapter 4: AMOC"
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
AMOC (At Most One Change) assumes a single changepoint and searches for the split that maximizes improvement in fit. It is a simple baseline for series with a single dominant change.
## Example Overview and Goals
We load a synthetic series, fit an AMOC detector, list the detected index, and visualize the result.
### Knitr Options
Keep output clean and reproducible.

### Setup and Libraries
Load helpers and packages.

``` r
library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")
```
### Data
Load the change-point example.

``` r
data(examples_changepoints)
data <- examples_changepoints$complex
```
### Model, Fit, and Detect
Instantiate AMOC, fit, and detect.

``` r
model <- fit(hcp_amoc(), data$serie)
detection <- detect(model, data$serie)
print(detection$idx[detection$event])
```

```
## [1] 389
```
### Visualization and Output
Plot detections and save the figure.

``` r
grf <- har_plot(model, data$serie, detection) + ylab("value") + font
#save_png(grf, "figures/chap4_amoc.png", 1280, 720)
grf
```

![plot of chunk plot](fig/chap4_amoc/plot-1.png)
## References
* Killick, R., Fearnhead, P., & Eckley, I. A. (2012). Optimal detection of changepoints with a linear computational cost.
* Ogasawara, E., Salles, R., Porto, F., Pacitti, E. Event Detection in Time Series. Springer, 2025. doi:10.1007/978-3-031-75941-3.

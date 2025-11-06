---
title: "Chapter 4: PELT"
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
Changepoint detection methods partition a series into piecewise-homogeneous segments by minimizing a cost plus a penalty that controls the number of changes. PELT (Pruned Exact Linear Time) solves this efficiently, achieving near-linear complexity under mild conditions.
## Example Overview and Goals
We load a synthetic change-point series, fit a PELT-based detector, run detection, print detected indices, and generate a figure.
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
Instantiate the PELT detector, fit, and detect events.

``` r
model <- fit(hcp_pelt(), data$serie)
detection <- detect(model, data$serie)
print(detection$idx[detection$event])
```

```
##  [1] 101 200 312 326 332 346 352 365 372 385 390
```
### Visualization and Output
Plot detections over the series and save the figure.

``` r
grf <- har_plot(model, data$serie, detection) + ylab("value") + font
#save_png(grf, "figures/chap4_pelt.png", 1280, 720)
grf
```

![plot of chunk plot](fig/chap4_pelt/plot-1.png)
## References
* Killick, R., Fearnhead, P., & Eckley, I. A. (2012). Optimal detection of changepoints with a linear computational cost.
* Ogasawara, E., Salles, R., Porto, F., Pacitti, E. Event Detection in Time Series. Springer, 2025. doi:10.1007/978-3-031-75941-3.

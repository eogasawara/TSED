---
title: "Chapter 4: Sliding Change Point (SCP)"
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
Sliding Change Point (SCP) scans a sliding window across the series, fitting local models to detect abrupt changes with minimal latency. It complements global search methods like PELT/BinSeg.
## Example Overview and Goals
We demonstrate a complete, reproducible workflow: setting up libraries, loading data, configuring a detector, fitting it, running detection, optionally evaluating, and visualizing the results.
### Knitr Options
Keep output clean and reproducible.

### Setup and Libraries
Load helpers and packages.

### Data
Load a simple change-point example.

``` r
data(examples_changepoints)
data <- examples_changepoints$simple
```
### Model, Fit, and Detect
Configure SCP with a window size suited to the change scale and run detection.

``` r
model <- fit(hcp_scp(sw_size = 30), data$serie)
detection <- detect(model, data$serie)
print(detection$idx[detection$event])
```

```
## [1] 50
```
### Visualization and Output
Plot the series with detected changes and save the figure.

``` r
grf <- har_plot(model, data$serie, detection) + ylab("value") + font
#save_png(grf, "figures/chap4_scp.png", 1280, 720)
grf
```

![plot of chunk plot](fig/chap4_scp/plot-1.png)
## References
* Killick, R., Fearnhead, P., & Eckley, I. A. (2012). Optimal detection of changepoints with a linear computational cost.
* Ogasawara, E., Salles, R., Porto, F., Pacitti, E. Event Detection in Time Series. Springer, 2025. doi:10.1007/978-3-031-75941-3.

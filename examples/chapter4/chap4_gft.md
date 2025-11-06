---
title: "Chapter 4: Gft"
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
This example focuses on Event detection in time series. This example follows a general event-detection workflow: data preparation, model configuration, fitting, detection, optional evaluation, and visualization.
## Example Overview and Goals
We demonstrate a complete, reproducible workflow: setting up libraries, loading data, configuring a detector, fitting it, running detection, optionally evaluating, and visualizing the results.
### Setup and Libraries
Load helpers and required packages.

``` r
library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")
```
### Data
Load the example series.

``` r
data(examples_changepoints)
```

``` r
data <- examples_changepoints$complex
model <- fit(hcp_gft(), data$serie)
```
### Event Detection

``` r
detection <- detect(model, data$serie)
print(detection$idx[detection$event])
```

```
## [1] 200 307 391
```
### Visualization and Output

``` r
grf <- har_plot(model, data$serie, detection) + ylab("value") + font
#save_png(grf, "figures/chap4_gft.png", 1280, 720)
grf
```

![plot of chunk plot](fig/chap4_gft/plot-1.png)
## References
* General: Box, G. E. P. & Jenkins, G. (1970). Time Series Analysis.
* Ogasawara, E., Salles, R., Porto, F., Pacitti, E. Event Detection in Time Series. Springer, 2025. doi:10.1007/978-3-031-75941-3.

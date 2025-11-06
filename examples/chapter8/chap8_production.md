---
title: "Chapter 8: Production"
output: html_document
---

``` r
library(RefManageR)
library(tibble)
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(scales)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")
```
## Theoretical Overview
This chapter visualizes bibliometric production over time across event-related topics (anomaly, change point, motif, event detection/prediction), highlighting growth patterns.
## Example Overview and Goals
We demonstrate a complete, reproducible workflow: setting up libraries, loading data, configuring a detector, fitting it, running detection, optionally evaluating, and visualizing the results.
### Knitr Options
Keep output clean and reproducible.

### Setup and Libraries
Short rationale for the libraries and any project-specific sources.

``` r
# Color palette for multi-series plots (four distinct hues)
colors <- brewer.pal(9, "Set1")[c(1,2,3,4)]
```
### Data Loading and Prep
Read the dataset and perform any minimal preparation required for modeling.

``` r
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/event_detection.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/event_prediction.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/anomalies.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/change_point.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/concept_drift.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/motif.RData"))
```
### Other Steps
Additional supporting steps that glue the workflow.

``` r
papers_by_year <- function(data, name) {
  data$key <- row.names(data)
  data$title <- str_to_lower(data$title)
  data <- data |> dplyr::select(key, type, year, title)
  data$year <- as.numeric(data$year)
  data$year[data$year > 2023] <- 2023
  data <- data |> dplyr::group_by(year) |> dplyr::summarise(qtd = n()) |> dplyr::arrange(year)
  data$name <- name
  data <- data |> select(year, name, qtd)
  return(data)
}
data <- NULL
result <- papers_by_year(anomalies, "anomaly")
data <- rbind(result, data)
result <- papers_by_year(rbind(change_point, concept_drift), "change point")
data <- rbind(result, data)
result <- papers_by_year(motif, "motif")
data <- rbind(result, data)
result <- papers_by_year(rbind(event_detection, event_prediction), "event detection")
data <- rbind(result, data)
area_year <- tidyr::pivot_wider(data, names_from = "name", values_from="qtd") 
area_year <- area_year |> dplyr::arrange(year)
colnames(area_year)[1] <- "x"
head(area_year)
```

```
## # A tibble: 6 × 5
##       x `event detection` motif `change point` anomaly
##   <dbl>             <int> <int>          <int>   <int>
## 1  1954                NA    NA             NA       1
## 2  1968                NA    NA             NA       1
## 3  1969                NA    NA             NA       1
## 4  1973                NA    NA             NA       1
## 5  1974                NA    NA             NA       1
## 6  1975                NA    NA             NA       3
```

``` r
# Build log-scaled production curves; expand x-axis for visual margin
invisible(seq(1950, 2020, 10))
grf <- plot_series(area_year, colors = colors) + scale_y_continuous(trans = log10_trans())
grf <- grf + font 
grf <- grf + scale_x_continuous(expand = expansion(add = 10))
```
### Visualization and Output
Plot production curves (log-scaled) and save the figure.

``` r
#save_png(grf, "figures/chap8_production.png", 1280, 720)
grf
```

![plot of chunk unnamed-chunk-3](fig/chap8_production/unnamed-chunk-3-1.png)
## References
* General: Box, G. E. P. & Jenkins, G. (1970). Time Series Analysis.
* Ogasawara, E., Salles, R., Porto, F., Pacitti, E. Event Detection in Time Series. Springer, 2025. doi:10.1007/978-3-031-75941-3.

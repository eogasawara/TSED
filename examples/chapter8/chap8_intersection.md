---
title: "Chapter 8: Intersection"
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
```
## Theoretical Overview
This chapter quantifies intersections (overlaps) among bibliographic corpora for event-related topics: how often the same papers appear across categories.
## Example Overview and Goals
We demonstrate a complete, reproducible workflow: setting up libraries, loading data, configuring a detector, fitting it, running detection, optionally evaluating, and visualizing the results.
### Knitr Options
Keep output clean and reproducible.

### Setup and Libraries
Short rationale for the libraries and any project-specific sources.

### Data Loading and Prep
Load curated datasets and compute pairwise intersections as counts.

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
intersection <- function(event_detection, event_prediction, anomalies, change_point, concept_drift, motif) {
  icount <- function(name, x, y) {
    xy <- merge(x, y)
    data <- data.frame(variable = name, qtd = nrow(xy))
    return(data)
  }
  change_point <- rbind(change_point, concept_drift)
  event_detection <- rbind(event_detection, event_prediction)
  data <- NULL
  data <- rbind(data, icount("event detection - anomaly", event_detection, anomalies))
  data <- rbind(data, icount("event detection - change point", event_detection, change_point))
  data <- rbind(data, icount("event detection - motif", event_detection, motif))
  data <- rbind(data, icount("anomaly - change point", anomalies, change_point))
  data <- rbind(data, icount("anomaly - motif", anomalies, motif))
  data <- rbind(data, icount("change point - motif", change_point, motif))
  return(data)
}
data <- intersection(event_detection, event_prediction, anomalies, change_point, concept_drift, motif)
print(head(data))
```

```
##                         variable qtd
## 1      event detection - anomaly  89
## 2 event detection - change point  13
## 3        event detection - motif   4
## 4         anomaly - change point 215
## 5                anomaly - motif  65
## 6           change point - motif   7
```
## References
* General: Box, G. E. P. & Jenkins, G. (1970). Time Series Analysis.
* Ogasawara, E., Salles, R., Porto, F., Pacitti, E. Event Detection in Time Series. Springer, 2025. doi:10.1007/978-3-031-75941-3.

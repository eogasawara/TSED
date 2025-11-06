---
title: "Chapter 8: Summary"
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
This chapter summarizes bibliographic coverage across event-related topics (anomaly, change point, motif, and event detection/prediction) and publication types.
## Example Overview and Goals
We load curated datasets, compute counts by publication type for each topic, and present a tidy summary suitable for downstream plotting or reporting.
### Knitr Options
Keep output clean and reproducible.

### Setup and Libraries
Load tidyverse-style helpers for data wrangling and plotting.

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
### Data Loading
Load preprocessed bibliographic data from the repository.

``` r
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/event_detection.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/event_prediction.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/anomalies.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/change_point.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/concept_drift.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/motif.RData"))
```
### Summaries by Publication Type
Utility to count entries by type for each topic, with simple normalization.

``` r
# Count entries by publication type for a given topic name
papers_by_type <- function(data, name) {
  data$key <- row.names(data)
  data$title <- str_to_lower(data$title)
  data <- data |> dplyr::select(key, type, year, title)
  data <- data |> dplyr::group_by(type) |> dplyr::summarise(qtd = n()) |> dplyr::arrange(desc(qtd))
  data$name <- name
  data |> dplyr::select(type, name, qtd)
}
summary_tbl <- NULL
summary_tbl <- rbind(papers_by_type(anomalies, "anomaly"), summary_tbl)
summary_tbl <- rbind(papers_by_type(rbind(change_point, concept_drift), "change point"), summary_tbl)
summary_tbl <- rbind(papers_by_type(motif, "motif"), summary_tbl)
summary_tbl <- rbind(papers_by_type(rbind(event_detection, event_prediction), "event detection"), summary_tbl)
# Wider table by topic; keep publication types of interest
publication_type <- tidyr::pivot_wider(summary_tbl, names_from = "name", values_from = "qtd") |>
  dplyr::filter(type %in% c("Article", "Conference paper", "Review", "Book"))
print(head(publication_type))
```

```
## # A tibble: 4 × 5
##   type             `event detection` motif `change point` anomaly
##   <chr>                        <int> <int>          <int>   <int>
## 1 Article                        364   388           1719    6679
## 2 Conference paper               324   296            555    2825
## 3 Review                           5     9             29     110
## 4 Book                            NA    NA              7      21
```
## References
* General: Box, G. E. P. & Jenkins, G. (1970). Time Series Analysis.
* Ogasawara, E., Salles, R., Porto, F., Pacitti, E. Event Detection in Time Series. Springer, 2025. doi:10.1007/978-3-031-75941-3.

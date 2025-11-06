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

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

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

load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/event_detection.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/event_prediction.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/anomalies.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/change_point.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/concept_drift.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/motif.RData"))

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

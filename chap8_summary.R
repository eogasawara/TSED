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

load("data/references/event_detection.RData")
load("data/references/event_prediction.RData")
load("data/references/anomalies.RData")
load("data/references/change_point.RData")
load("data/references/concept_drift.RData")
load("data/references/motif.RData")

papers_by_type <- function(data, name) {
  data$key <- row.names(data)
  data$title <- str_to_lower(data$title)
  data <- data |> dplyr::select(key, type, year, title)
  data <- data |> dplyr::group_by(type) |> dplyr::summarise(qtd = n()) |> dplyr::arrange(desc(qtd))
  data$name <- name
  data <- data |> select(type, name, qtd)
  return(data)
}

data <- NULL
result <- papers_by_type(anomalies, "anomaly")
data <- rbind(result, data)

result <- papers_by_type(rbind(change_point, concept_drift), "change point")
data <- rbind(result, data)

result <- papers_by_type(motif, "motif")
data <- rbind(result, data)

result <- papers_by_type(rbind(event_detection, event_prediction), "event detection")
data <- rbind(result, data)

publication_type <- tidyr::pivot_wider(data, names_from = "name", values_from="qtd") |>
  dplyr::filter(type %in% c("Article","Conference paper","Review","Book")) 
print(head(publication_type))


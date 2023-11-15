source("header.R")
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

grf <- plot_series(area_year, colors=colors[1:4]) + scale_y_continuous(trans = log10_trans())
grf <- grf + font

save_png(grf, "figures/chap8_production.png", 1280, 720)


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



load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/event_detection.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/event_prediction.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/anomalies.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/change_point.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/concept_drift.RData"))
load(url("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/data/motif.RData"))

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

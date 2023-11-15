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
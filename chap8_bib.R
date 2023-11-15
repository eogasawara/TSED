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

anomalies <- ReadBib("data/references/anomaly AND time series.bib", check = FALSE)
change_point <- ReadBib("data/references/change point AND time series.bib", check = FALSE)
concept_drift <- ReadBib("data/references/concept drift AND time series.bib", check = FALSE)
motif <- ReadBib("data/references/motif AND time series.bib", check = FALSE)
event_detection <- ReadBib("data/references/event detection AND time series.bib", check = FALSE)
event_prediction <- ReadBib("data/references/event prediction AND time series.bib", check = FALSE)

anomalies <- as.data.frame(anomalies)
save(anomalies, file="data/references/anomalies.RData", compress=TRUE)

change_point <- as.data.frame(change_point)
change_point <- change_point[,1:17]
save(change_point, file="data/references/change_point.RData", compress=TRUE)

concept_drift <- as.data.frame(concept_drift)
save(concept_drift, file="data/references/concept_drift.RData", compress=TRUE)

motif <- as.data.frame(motif)
save(motif, file="data/references/motif.RData", compress=TRUE)

event_detection <- as.data.frame(event_detection)
event_detection <- event_detection[,1:17]
save(event_detection, file="data/references/event_detection.RData", compress=TRUE)

event_prediction <- as.data.frame(event_prediction)
save(event_prediction, file="data/references/event_prediction.RData", compress=TRUE)

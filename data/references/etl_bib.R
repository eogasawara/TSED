# Chapter 8: Bib
# Overview:
# - Loads example dataset (if applicable)
# - Fits a detector and runs event detection
# - Plots results with events overlaid
#
# How to run:
# setwd("chapter8"); source("chap8_bib.R")
#

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

anomalies <- ReadBib("anomaly AND time series.bib", check = FALSE)
change_point <- ReadBib("change point AND time series.bib", check = FALSE)
concept_drift <- ReadBib("concept drift AND time series.bib", check = FALSE)
motif <- ReadBib("motif AND time series.bib", check = FALSE)
event_detection <- ReadBib("event detection AND time series.bib", check = FALSE)
event_prediction <- ReadBib("event prediction AND time series.bib", check = FALSE)

anomalies <- as.data.frame(anomalies)
save(anomalies, file="anomalies.RData", compress=TRUE)

change_point <- as.data.frame(change_point)
change_point <- change_point[,1:17]
save(change_point, file="change_point.RData", compress=TRUE)

concept_drift <- as.data.frame(concept_drift)
save(concept_drift, file="concept_drift.RData", compress=TRUE)

motif <- as.data.frame(motif)
save(motif, file="motif.RData", compress=TRUE)

event_detection <- as.data.frame(event_detection)
event_detection <- event_detection[,1:17]
save(event_detection, file="event_detection.RData", compress=TRUE)

event_prediction <- as.data.frame(event_prediction)
save(event_prediction, file="event_prediction.RData", compress=TRUE)



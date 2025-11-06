library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

# Load shared helpers (plot theme, save helpers, etc.)
# Core toolboxes used throughout the book/examples

# Load collection of example datasets from harbinger
data(examples_harbinger)
# Select yearly global temperature series and initialize event labels
data <- examples_harbinger$global_temperature_yearly
data$event <- FALSE  # ground-truth labels (none in this example)

# Create a generic Harbinger detector (auto-selects sensible defaults)
model <- harbinger()

# Fit the detector on the univariate time series
model <- fit(model, data$serie)

# Produce detection results; detection$event is a logical vector
detection <- detect(model, data$serie)

# Create a visualization overlaying detections on the original series
grf <- har_plot(model, data$serie, detection, data$event, idx = data$i) +
  font +
  scale_x_date(
    breaks = "10 years",
    date_labels = "%Y",
    limits = c(as.Date("1850-01-01"), as.Date("2030-01-01"))
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# Save image for reproducibility
#save_png(grf, "figures/chap1_ygt.png", 1280, 720)
grf

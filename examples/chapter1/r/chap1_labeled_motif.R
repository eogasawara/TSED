library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

# Shared helpers (themes, saving utilities, etc.)

# Load example motif datasets
data(examples_motifs)
# Select a labeled ECG time series (MIT-BIH record 102)
data <- examples_motifs$mitdb102
rownames(data) <- 1:nrow(data)  # ensure a simple, sequential index

# Fit a default Harbinger detector and detect candidate events
model <- fit(harbinger(), data$serie)
detection <- detect(model, data$serie)

# Add optional descriptive fields: type, sequence id, and sequence length
detection$type <- NA
detection$seq <- NA
detection$seqlen <- NA
# Mark labeled events as motifs for clarity in downstream visualization
detection$event[data$event] <- TRUE
detection$type[data$event] <- "motif"
detection$seq[data$event] <- 1
detection$seqlen[data$event] <- 50
# Show only detected (or labeled) events
print(detection[detection$event, ])

grf <- har_plot(model, data$serie, detection, data$event) +
  font +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
#save_png(grf, "figures/chap1_labeled_motif.png", 1280, 720)
grf

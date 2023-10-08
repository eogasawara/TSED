source("header.R")
options(scipen=999)
library(ggpmisc)

library(stats)


#loading the example database
data(har_examples)

#Using the time series 18 
dataset <- har_examples$example18
dataset$event <- factor(dataset$event, labels=c("FALSE", "TRUE"))
head(dataset)

ts_data <- dataset$serie

# Create a histogram of the time series data
hist_data <- hist(ts_data[1:75], plot = FALSE)

# Calculate bin edges and midpoints
bin_edges <- hist_data$breaks

# Define a threshold for anomaly detection (adjust this as needed)
threshold <- 2.5  # Adjust this as needed

#ts_data <- rnorm(n)

# Detect anomalies based on the histogram
anomalies <- numeric(length(ts_data))
for (i in 1:length(ts_data)) {
  # Find the bin to which the data point belongs
  bin_index <- findInterval(ts_data[i], bin_edges)
  
  # Calculate the expected range (bin boundaries)
  if (bin_index < 1)
    bin_index <- 1
  lower_bound <- bin_edges[bin_index]
  if (bin_index < length(bin_edges))
    upper_bound <- bin_edges[bin_index+1]
  else
    upper_bound <- bin_edges[bin_index]
  
  # Check if the data point is outside the expected range
  if (ts_data[i] < lower_bound || ts_data[i] > upper_bound || hist_data$density[bin_index] < 0.05) {
    anomalies[i] <- 1  # Mark the data point as an anomaly
  }
}

# Print the indices of detected anomalies
anomaly_indices <- which(anomalies == 1)
if (length(anomaly_indices) > 0) {
  cat("Anomalies detected at time indices:", anomaly_indices, "\n")
} else {
  cat("No anomalies detected.\n")
}


colors <- rep("white", length(hist_data$density))
colors[hist_data$density < 0.05] <- "red"

grfHist <- plot_hist(dataset[1:75, 1, drop=FALSE], 
                 label_x = " ", label_y = " ", color=colors) + font
grfHist <- grfHist + fontstyle + font


model <- harbinger()

# fitting the model
model <- fit(model, train_n$serie)
detection <- detect(model, dataset_n$serie)
detection$event[anomaly_indices] <- TRUE
detection$type[anomaly_indices] <- "anomaly"


# ploting training results
grf <- har_plot(model, dataset_n$serie, detection, as.logical(dataset_n$event))
grf <- grf + geom_vline(xintercept = 75, col = "black", linetype = "dashed")
grf <- grf + fontstyle + font

mypng(file="figures/chap3_histogram.png", width = 1600, height = 720) # 1280 * 1.5
gridExtra::grid.arrange(grf, grid::nullGrob(), grfHist, grid::nullGrob(), 
                        layout_matrix = matrix(c(1,1,1,1,1,1,2,2,1,1,1,1,1,1,3,3,1,1,1,1,1,1,3,3,1,1,1,1,1,1,4,4), 
                                               byrow = TRUE, ncol = 8))
dev.off() 


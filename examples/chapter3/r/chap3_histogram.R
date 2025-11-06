library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")



data("examples_anomalies")
dataset <- examples_anomalies$tt_warped
dataset$event <- factor(dataset$event, labels = c("FALSE", "TRUE"))
# Minor smoothing/cleanup of a few positions just for illustration
dataset$serie[1]  <- dataset$serie[1] - 0.001
dataset$serie[12] <- (dataset$serie[11] + dataset$serie[13]) / 2; dataset$event[12] <- FALSE
dataset$serie[50] <- (dataset$serie[49] + dataset$serie[51]) / 2; dataset$event[50] <- FALSE
dataset$serie[64] <- (dataset$serie[63] + dataset$serie[65]) / 2; dataset$event[64] <- FALSE
ts_data <- dataset$serie
train <- dataset[1:75, ]

# Build a histogram on the training slice to visualize low-density bins
hist_data <- hist(train$serie, plot = FALSE)
colors <- rep("white", length(hist_data$density))
colors[hist_data$density < 0.05] <- "red"
grfHist <- plot_hist(dataset[1:75, 1, drop = FALSE],
                     label_x = " ", label_y = " ", color = colors) + font
grfHist <- grfHist + xlab("(b)") + font

model <- hanr_histogram()

model <- fit(model, train$serie)

detection <- detect(model, dataset$serie)

grf <- har_plot(model, ts_data, detection, as.logical(dataset$event))

grf <- grf + geom_vline(xintercept = 75, col = "black", linetype = "dashed")
grf <- grf + xlab("(a)") + font

#mypng(file = "figures/chap3_histogram.png", width = 1600, height = 720)
gridExtra::grid.arrange(grf, grid::nullGrob(), grfHist, grid::nullGrob(),
                        layout_matrix = matrix(c(1,1,1,1,1,1,2,2,
                                                 1,1,1,1,1,1,3,3,
                                                 1,1,1,1,1,1,3,3,
                                                 1,1,1,1,1,1,4,4),
                                               byrow = TRUE, ncol = 8))
#dev.off()

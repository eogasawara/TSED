source("header.R")
library(daltoolbox)
library(harbinger)

#loading the example database
data(har_examples)

#Using the time series 18 
dataset <- har_examples$example18
dataset$serie[1] <- dataset$serie[1] - 0.001
dataset$serie[12] <- (dataset$serie[11]+dataset$serie[13])/2
dataset$event[12] <- FALSE
#dataset$serie[24] <- (dataset$serie[23]+dataset$serie[25])/2
#dataset$event[24] <- FALSE
dataset$serie[50] <- (dataset$serie[49]+dataset$serie[51])/2
dataset$event[50] <- FALSE
dataset$serie[64] <- (dataset$serie[63]+dataset$serie[65])/2
dataset$event[64] <- FALSE
dataset$event <- factor(dataset$event, labels=c("FALSE", "TRUE"))
head(dataset)

ts_data <- dataset$serie

train <- dataset[1:75,]
test <- dataset[-(1:75),]

# Create a histogram of the time series data
hist_data <- hist(train$serie, plot = FALSE)

# Calculate bin edges and midpoints
bin_edges <- hist_data$breaks

colors <- rep("white", length(hist_data$density))
colors[hist_data$density < 0.05] <- "red"

grfHist <- plot_hist(dataset[1:75, 1, drop=FALSE], 
                 label_x = " ", label_y = " ", color=colors) + font
grfHist <- grfHist  + font
grfHist <- grfHist + xlab("(b)")


model <- hanr_histogram()
model <- fit(model, train$serie)
detection <- detect(model, dataset$serie)

# ploting training results
grf <- har_plot(model, ts_data, detection, as.logical(dataset$event))
grf <- grf + geom_vline(xintercept = 75, col = "black", linetype = "dashed")
grf <- grf + xlab("(a)")
grf <- grf  + font

mypng(file="figures/chap3_histogram.png", width = 1600, height = 720) # 1280 * 1.5
gridExtra::grid.arrange(grf, grid::nullGrob(), grfHist, grid::nullGrob(), 
                        layout_matrix = matrix(c(1,1,1,1,1,1,2,2,1,1,1,1,1,1,3,3,1,1,1,1,1,1,3,3,1,1,1,1,1,1,4,4), 
                                               byrow = TRUE, ncol = 8))
dev.off() 


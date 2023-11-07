source("header.R")
library(daltoolbox)
library(harbinger)

#loading the example database
data(har_examples)

#Using the time series 18 
dataset <- har_examples$example18
dataset$event <- factor(dataset$event, labels=c("FALSE", "TRUE"))
head(dataset)

ts_data <- dataset$serie

train <- dataset[1:75,]
test <- dataset[-(1:75),]

norm <- daltoolbox::minmax()
norm <- fit(norm, train)
train_n <- transform(norm, train)
dataset_n <- transform(norm, dataset)
summary(train_n)

# Create a histogram of the time series data
hist_data <- hist(train$serie, plot = FALSE)

# Calculate bin edges and midpoints
bin_edges <- hist_data$breaks

colors <- rep("white", length(hist_data$density))
colors[hist_data$density < 0.05] <- "red"

grfHist <- plot_hist(dataset[1:75, 1, drop=FALSE], 
                 label_x = " ", label_y = " ", color=colors) + font
grfHist <- grfHist  + font


model <- hanr_histogram()
model <- fit(model, train_n$serie)
detection <- detect(model, dataset_n$serie)

# ploting training results
grf <- har_plot(model, dataset_n$serie, detection, as.logical(dataset_n$event))
grf <- grf + geom_vline(xintercept = 75, col = "black", linetype = "dashed")
grf <- grf  + font

mypng(file="figures/chap3_histogram.png", width = 1600, height = 720) # 1280 * 1.5
gridExtra::grid.arrange(grf, grid::nullGrob(), grfHist, grid::nullGrob(), 
                        layout_matrix = matrix(c(1,1,1,1,1,1,2,2,1,1,1,1,1,1,3,3,1,1,1,1,1,1,3,3,1,1,1,1,1,1,4,4), 
                                               byrow = TRUE, ncol = 8))
dev.off() 


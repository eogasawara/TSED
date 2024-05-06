source("header.R")
library(daltoolbox)
library(harbinger)


#loading the example database
data("examples_anomalies")

#Using the tt warped time series
dataset <- dataset <- examples_anomalies$tt_warped
dataset$event <- factor(dataset$event, labels=c("FALSE", "TRUE"))
head(dataset)

#ploting serie #1
plot_ts(x = 1:length(dataset$serie), y = dataset$serie)

# data preprocessing
slevels <- levels(dataset$event)

train <- dataset[1:75,]
test <- dataset[-(1:75),]

model <- harbinger()

# fitting the model
model <- fit(model, train$serie)
detection <- detect(model, dataset$serie)

# ploting training results
grf <- har_plot(model, dataset$serie, detection, as.logical(dataset$event))
grf <- grf + geom_vline(xintercept = 75, col = "black", linetype = "dashed")
grf <- grf  + font
save_png(grf, "figures/chap3_example.png", 1280, 720)


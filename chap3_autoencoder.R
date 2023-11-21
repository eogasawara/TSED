source("header.R")
library(daltoolbox)
library(harbinger)

#loading the example database
data(har_examples)

#Using the time series 18 
dataset <- har_examples$example18
dataset$event <- factor(dataset$event, labels=c("FALSE", "TRUE"))
head(dataset)

#ploting serie #1
plot_ts(x = 1:length(dataset$serie), y = dataset$serie)

# data preprocessing
slevels <- levels(dataset$event)

model <- han_autoencoder(3,1)

# fitting the model
model <- fit(model, dataset$serie)
detection <- detect(model, dataset$serie)

# ploting training results
grf <- har_plot(model, dataset$serie, detection, as.logical(dataset$event))
grf <- grf + geom_vline(xintercept = 75, col = "black", linetype = "dashed")
grf <- grf  + font
save_png(grf, "figures/chap3_autoencoder.png", 1280, 720)

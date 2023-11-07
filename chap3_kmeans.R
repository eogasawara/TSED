source("header.R")
library(daltoolbox)
library(harbinger)


#loading the example database
data(har_examples)

#Using the time series 18
dataset <- har_examples$example18
head(dataset)

#ploting serie #18
plot_ts(x = 1:length(dataset$serie), y = dataset$serie)

model <- hanct_kmeans(1)

# fitting the model
model <- fit(model, dataset$serie)

# making detections of anomalies using kmeans
detection <- detect(model, dataset$serie)

# filtering detected events
print(detection |> dplyr::filter(event==TRUE))

# evaluating the detections

evaluation <- daltoolbox::evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)

# ploting the results
grf <- har_plot(model, dataset$serie, detection, dataset$event)
grf <- grf + geom_vline(xintercept = 75, col = "black", linetype = "dashed")
grf <- grf  + font
save_png(grf, "figures/chap3_kmeans.png", 1280, 720)



source("header.R")
options(scipen=999)
library(ggpmisc)


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

train <- dataset[1:75,]
test <- dataset[-(1:75),]

norm <- minmax()
norm <- fit(norm, train)
train_n <- transform(norm, train)
dataset_n <- transform(norm, dataset)
summary(train_n)

model <- hanc_ml(cla_svm("event", slevels, epsilon=0.0,cost=20.000))

# fitting the model
model <- fit(model, train_n)
detection <- detect(model, dataset_n)

# ploting training results
grf <- har_plot(model, dataset_n$serie, detection, as.logical(dataset_n$event))
grf <- grf + geom_vline(xintercept = 75, col = "black", linetype = "dashed")
grf <- grf + fontstyle + font
save_png(grf, "figures/chap3_svm.png", 1280, 720)
source("header.R")
options(scipen=999)
library(ggpmisc)
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

train <- dataset[1:75,]
test <- dataset[-(1:75),]

norm <- daltoolbox::minmax()
norm <- fit(norm, train)
train_n <- transform(norm, train)
dataset_n <- transform(norm, dataset)
summary(train_n)

model <- hanr_arima()

# fitting the model
model <- fit(model, train_n$serie)
detection <- detect(model, dataset_n$serie)


# DAL ToolBox
# version 1.0.707

ts <- ts_data(dataset_n$serie, 0)
io <- ts_projection(ts)

model <- ts_arima()
model <- fit(model, x=io$input, y=io$output)

adjust <- predict(model, io$input)
adjust <- as.vector(adjust)

# ploting training results
grf <- har_plot(model, dataset_n$serie, detection, as.logical(dataset_n$event))
grf <- grf + geom_vline(xintercept = 75, col = "black", linetype = "dashed")
grf <- grf + geom_line(aes(y=adjust), linetype = "dashed", col="darkblue") 
grf <- grf + geom_point(aes(y=adjust), size=0.25, col="darkblue") 
grf <- grf  + font
save_png(grf, "figures/chap3_arima.png", 1280, 720)





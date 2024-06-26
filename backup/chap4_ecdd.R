source("header.R")
library(daltoolbox)
library(harbinger)
library(heimdall)


data(examples_changepoints)
data <- examples_changepoints$complex
data$event <- NULL
data$prediction <- examples_changepoints$complex$serie > 4


model <- dfr_ecdd(lambda = 0.2, min_run_instances = 50, average_run_length = 100)

detection <- c()
output <- list(obj=model, pred=FALSE)
for (i in 1:length(data$serie)){
  output <- update_state(output$obj, data$prediction[i])
  if (output$pred){
    type <- 'changepoint'
    output$obj <- reset_state(output$obj)
  }else{
    type <- ''
  }
  detection <- rbind(detection, list(idx=i, event=output$pred, type=type))
}

detection <- as.data.frame(detection)

grf <- har_plot(model, data$serie, detection)
grf <- grf + ylab("value")
grf

save_png(grf, "backup/chap4_ecdd.png", 1280, 720)
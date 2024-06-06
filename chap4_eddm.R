source("header.R")
library(daltoolbox)
library(harbinger)
library(heimdall)

data(examples_changepoints)
data <- examples_changepoints$complex
data$event <- NULL
data$prediction <- examples_changepoints$complex$serie > 4


model <- dfr_eddm()

detection <- c()
output <- list(obj=model, pred=FALSE)
for (i in 1:length(data$prediction)){
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

save_png(grf, "figures/chap4_eddm.png", 1280, 720)
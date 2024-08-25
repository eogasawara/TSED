source("header.R")
options(scipen=999)
library(ggpmisc)

data(examples_harbinger)
data <- examples_harbinger$global_temperature_yearly
data$event <- FALSE

#loading DAL
library(daltoolbox)

x <- data$serie
sw_size <- 10
ts <- ts_data(x, sw_size)
ts_head(ts, 3)

test_size <- 4
samp <- ts_sample(ts, test_size)
ts_head(samp$train, 3)
ts_head(samp$test)

preproc <- ts_norm_gminmax()

{
  model <- ts_lstm(preproc, input_size=4, epochs=10000)
  io_train <- ts_projection(samp$train)
  model <- fit(model, x=io_train$input, y=io_train$output)

  adjust <- predict(model, io_train$input)
  adjust <- as.vector(adjust)
  output <- as.vector(io_train$output)
  ev_adjust <- daltoolbox::evaluate(model, output, adjust)
  print(head(ev_adjust$metrics))
  
  steps_ahead <- 4
  io_test <- ts_projection(samp$test)
  prediction <- predict(model, x=io_test$input[1,], steps_ahead=steps_ahead)
  prediction <- as.vector(prediction)
  
  output <- as.vector(io_test$output)
  if (steps_ahead > 1)
    output <- output[1:steps_ahead]
  
  print(sprintf("%.2f, %.2f", output, prediction))
  
  ev_test <- daltoolbox::evaluate(model, output, prediction)
  print(head(ev_test$metrics))  
  
  yvalues <- c(io_train$output, io_test$output)
  
  
  yts <- c(rep(NA, 9),yvalues)
  yts <- yts[-c(1:100)]
  yhat <- c(rep(NA, 9), adjust, prediction)
  yhat <- yhat[-c(1:100)]
  yts <- ts(yts, frequency=1, start = c(1950, 1))
  yhat <- ts(yhat, frequency=1, start = c(1950, 1))

  yhatadj <- yhat
  yhatadj[(length(yhat)-length(prediction)+1):(length(yhat))] <- NA
  yhatpred <- yhat
  yhatpred[1:(length(yhat)-length(prediction))] <- NA
  
  grf <- autoplot(yts, col="black")
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(plot.title = element_blank())
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + ylab("temperature")
  grf <- grf + xlab("time")
  grf <- grf + geom_point(aes(y=yts),size = 0.5, col="black") 
  grf <- grf + geom_line(aes(y=yhatadj), col="darkblue", linetype = "dashed") 
  grf <- grf + geom_point(aes(y=yhatadj), size = 0.5, col="darkblue") 
  grf <- grf + geom_line(aes(y=yhatpred), col="red", linetype = "dashed") 
  grf <- grf + geom_point(aes(y=yhatpred), size = 0.5, col="red") 
  grf <- grf + labs(caption = sprintf("(a) LSTM four-step-ahead prediction")) 
  grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
  grf <- grf  + font
  grfA <- grf
}


save_png(grfA, "figures/chap2_lstm.png", 1280, 720)









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
  ev_adjust$mse
  
  steps_ahead <- 4
  io_test <- ts_projection(samp$test)
  prediction <- predict(model, x=io_test$input[1,], steps_ahead=steps_ahead)
  prediction <- as.vector(prediction)
  
  output <- as.vector(io_test$output)
  if (steps_ahead > 1)
    output <- output[1:steps_ahead]
  
  print(sprintf("%.2f, %.2f", output, prediction))
  
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


{
  model_fit <- ts_lstm(preproc, input_size=4, epochs=10000)
  io_fit <- ts_projection(ts)
  model_fit <- fit(model_fit, x=io_fit$input, y=io_fit$output)
  
  adjust_fit <- predict(model_fit, io_fit$input)
  ev_adjust <- daltoolbox::evaluate(model, io_fit$output, adjust_fit)
  adjust_fit <- as.vector(adjust_fit)
  ev_adjust$mse
  
  
  yts_fit <- c(rep(NA, 9),io_fit$output)
  yts_fit <- yts_fit[-c(1:100)]
  adjust_fit <- c(rep(NA, 9), adjust_fit)
  adjust_fit <- adjust_fit[-c(1:100)]
  yts_fit <- ts(yts_fit, frequency=1, start = c(1950, 1))
  yhat_fit <- ts(adjust_fit, frequency=1, start = c(1950, 1))

  grf <- autoplot(yts_fit, col="black")
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(plot.title = element_blank())
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + ylab("temperature")
  grf <- grf + xlab("time")
  grf <- grf + geom_line(aes(y=yhat_fit), col="darkblue", linetype = "dashed") 
  grf <- grf + geom_point(aes(y=yhat_fit), size = 0.5, col="darkblue") 
  grf <- grf + geom_point(aes(y=yts_fit), size = 0.5, col="black") 
  grf <- grf + labs(caption = sprintf("(b) LSTM model adjustment")) 
  grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
  grf <- grf  + font
  grfB <- grf
}

mypng(file="figures/chap2_lstm.png", width = 1280, height = 1080) 
gridExtra::grid.arrange(grfA, grfB, 
                        layout_matrix = matrix(c(1,2), byrow = TRUE, ncol = 1))
dev.off() 












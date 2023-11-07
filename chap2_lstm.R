source("header.R")
options(scipen=999)
library(ggpmisc)

load("data/noaa-global/temp_yearly.RData")
data <- temp_yearly
data$event <- FALSE

#loading DAL
library(daltoolbox)

x <- temp_yearly$temperature
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
  
  yts <- ts(c(rep(NA, 9), yvalues), frequency=1, start = c(1850, 1))
  yhat <- ts(c(rep(NA, 9), adjust, prediction), frequency=1, start = c(1850, 1))
  yhatadj <- yhat
  yhatadj[(length(c(rep(NA, 9), adjust))+1):length(yhat)] <- NA
  yhatpred <- yhat
  yhatpred[1:(length(c(rep(NA, 9), adjust)))] <- NA
  
  grf <- autoplot(yts, col="black")
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(plot.title = element_blank())
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + ylab("temperature")
  grf <- grf + xlab("time")
  grf <- grf + geom_point(aes(y=yts),size = 0.5, col="black") 
  grf <- grf + geom_line(aes(y=yhatadj), col="darkblue", linetype = "dashed") 
  grf <- grf + geom_line(aes(y=yhatpred), col="red", linetype = "dashed") 
  grf <- grf + labs(caption = sprintf("(a) - LSTM four-step-ahead prediction")) 
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
  
  
  yvalues_fit <- c(io_fit$output)
  
  yts_fit <- ts(c(rep(NA, 9), yvalues_fit), frequency=1, start = c(1850, 1))
  yhat_fit <- ts(c(rep(NA, 9), adjust_fit), frequency=1, start = c(1850, 1))
  
  grf <- autoplot(yts_fit, col="black")
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(plot.title = element_blank())
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + ylab("temperature")
  grf <- grf + xlab("time")
  grf <- grf + geom_line(aes(y=yhat_fit), col="darkblue", linetype = "dashed") 
  grf <- grf + geom_point(aes(y=yts_fit), size = 0.5, col="black") 
  grf <- grf + labs(caption = sprintf("(b) - LSTM model adjustment")) 
  grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
  grf <- grf  + font
  grfB <- grf
}

mypng(file="figures/chap2_lstm.png", width = 1280, height = 1080) 
gridExtra::grid.arrange(grfA, grfB, 
                        layout_matrix = matrix(c(1,2), byrow = TRUE, ncol = 1))
dev.off() 












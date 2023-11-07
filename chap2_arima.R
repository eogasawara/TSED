source("header.R")
options(scipen=999)
library(ggpmisc)
library(daltoolbox)

load("data/noaa-global/temp_yearly.RData")
data <- temp_yearly
data$event <- FALSE

ts <- ts_data(data$temperature, 0)

test_size <- 4
samp <- ts_sample(ts, test_size)
ts_head(samp$train, 3)

model <- ts_arima()

io_train <- ts_projection(samp$train)
model <- daltoolbox::fit(model, x=io_train$input, y=io_train$output)

adjust <- predict(model, io_train$input)
ev_adjust <- daltoolbox::evaluate(model, io_train$output, adjust)
print(head(ev_adjust$metrics))

steps_ahead <- 4
io_test <- ts_projection(samp$test)
prediction <- predict(model, x=io_test$input, steps_ahead=steps_ahead)
prediction <- as.vector(prediction)

output <- as.vector(io_test$output)
if (steps_ahead > 1)
  output <- output[1:steps_ahead]

print(sprintf("%.2f, %.2f", output, prediction))

ev_test <- daltoolbox::evaluate(model, output, prediction)
print(head(ev_test$metrics))

yvalues <- c(io_train$output, io_test$output)

params <- attr(model, "params")
yts <- ts(data$temperature, frequency=1, start = c(1850, 1))
yhat <- ts(c(adjust, prediction), frequency=1, start = c(1850, 1))
yhatadj <- yhat
yhatadj[(length(adjust)+1):length(yhat)] <- NA
yhatpred <- yhat
yhatpred[1:(length(adjust)-1)] <- NA

grf <- autoplot(yts, col="black")
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + geom_point(aes(y=yts),size = 0.5, col="black") 
grf <- grf + geom_line(aes(y=yhatadj), col="darkblue", linetype = "dashed") 
grf <- grf + geom_line(aes(y=yhatpred), col="red", linetype = "dashed") 
grf <- grf + labs(caption = sprintf("(a) - ARIMA(%d, %d, %d) four-step-ahead prediction", params$p, params$d, params$q)) 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfA <- grf


model_fit <- ts_arima()
io_train_fit <- ts_projection(ts)
model_fit <- daltoolbox::fit(model_fit, x=io_train_fit$input, y=io_train_fit$output)
adjust_fit <- predict(model_fit, io_train_fit$input)
params_fit <- attr(model_fit, "params")
yts_fit <- ts(data$temperature, frequency=1, start = c(1850, 1))
yhat_fit <- ts(adjust_fit, frequency=1, start = c(1850, 1))

grf <- autoplot(yts_fit, col="black")
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + geom_line(aes(y=yhat_fit), col="darkblue", linetype = "dashed") 
grf <- grf + geom_point(aes(y=yts_fit),size = 1, col="black") 
grf <- grf + labs(caption = sprintf("(b) - ARIMA(%d, %d, %d) model adjustment", params_fit$p, params_fit$d, params_fit$q)) 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfB <- grf

mypng(file="figures/chap2_arima.png", width = 1280, height = 1080) 
gridExtra::grid.arrange(grfA, grfB, 
                        layout_matrix = matrix(c(1,2), byrow = TRUE, ncol = 1))
dev.off() 




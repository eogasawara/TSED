source("header.R")
options(scipen=999)
library(ggpmisc)
library(daltoolbox)
library(harbinger)

data(examples_harbinger)
data <- examples_harbinger$global_temperature_yearly
data$event <- FALSE

ts <- ts_data(data$serie, 0)

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
temperature <- data$serie[-c(1:100)]
adjust <- adjust[-c(1:100)]
yts <- ts(temperature, frequency=1, start = c(1950, 1))
yhat <- ts(c(adjust, prediction), frequency=1, start = c(1950, 1))
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
grf <- grf + geom_point(aes(y=yhatadj), size = 0.5, col="darkblue") 
grf <- grf + geom_line(aes(y=yhatpred), col="red", linetype = "dashed") 
grf <- grf + geom_point(aes(y=yhatpred), size = 0.5, col="red") 
grf <- grf + labs(caption = sprintf("(a) ARIMA(%d, %d, %d) four-step-ahead prediction", params$p, params$d, params$q)) 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfA <- grf


save_png(grfA, "figures/chap2_arima.png", 1280, 720)

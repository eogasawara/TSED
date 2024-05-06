source("header.R")
library(tseries)

data(examples_harbinger)
data <- examples_harbinger$global_temperature_yearly
data$event <- FALSE

bp.test <- function(serie) {
  library(lmtest)
  data <- data.frame(x = 1:length(serie), y = serie)
  fit <- lm(y ~ x, data = data)
  return(bptest(fit))
}

nonstationary.test <- function(serie) {
  return(data.frame(adf = round(adf.test(serie)$p.value, 2),
                    PP = round(PP.test(as.vector(serie))$p.value, 2),
                    bp = round(bp.test(serie)$p.value, 2)))
}

tsdata <- ts(data$serie, start = c(1850, 1))
model <- lm(tsdata ~ time(tsdata))
grf <- autoplot(tsdata, col="black")
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + geom_point(aes(y=tsdata), size = 0.25, col="black") 
grf <- grf + geom_line(aes(y=ts(model$fitted.values, start = c(1850, 1))), linetype = "dashed", col="darkblue") 
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + labs(caption = "(a)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfa <- grf

#MAS
y_transformed <- TSPred::mas(tsdata, 10)
yhat <- TSPred::mas.rev(y_transformed,  attr(y_transformed, "xi"), 10)
nonstationary.test(y_transformed)
y_transformed <- ts(c(rep(NA, 9), y_transformed), start = c(1850, 1))

grf <- autoplot(tsdata, col="black")
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + geom_point(aes(y=tsdata), size = 0.25, col="black") 
grf <- grf + geom_line(aes(y=y_transformed), linetype = "dashed", col="darkblue") 
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + labs(caption = "(b)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfb <- grf

mypng(file="figures/chap2_trend.png", width = 1600, height = 720) # 1280 * 1.5
gridExtra::grid.arrange(grfa, grfb, layout_matrix = matrix(c(1,2), byrow = TRUE, ncol = 2))
dev.off()  

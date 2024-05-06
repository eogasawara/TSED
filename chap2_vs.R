source("header.R")
library(tseries)
library(TSPred)

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

y <- data$serie

#log
y_transformed <- TSPred::LogT(y)
yhat <- TSPred::LogT.rev(y_transformed)
nonstationary.test(y_transformed)
serie <- ts(y_transformed, start = c(1850, 1))
grf <- autoplot(serie)
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + geom_point(aes(y=serie), size = 0.25, col="black") 
grf <- grf + ylab("LT")
grf <- grf + xlab("time")
grf <- grf + labs(caption = "(a)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
#plot(grf)
grfa <- grf

#BoxCox
y_transformed <- TSPred::BCT(y)
yhat <- BCT.rev(y_transformed,  attr(y_transformed, "lambda"))
nonstationary.test(y_transformed)
seriebc <- ts(y_transformed, start = c(1850, 1))
grf <- autoplot(seriebc)
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + geom_point(aes(y=seriebc), size = 0.25, col="black") 
grf <- grf + ylab("BCT")
grf <- grf + xlab("time")
grf <- grf + labs(caption = "(b)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfb <- grf

mypng(file="figures/chap2_vs.png", width = 1600, height = 720) # 1280 * 1.5
gridExtra::grid.arrange(grfa, grfb, layout_matrix = matrix(c(1,2), byrow = TRUE, ncol = 2))
dev.off() 



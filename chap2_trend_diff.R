source("header.R")
library(tseries)

load("data/noaa-global/temp_yearly.RData")
data <- temp_yearly
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

y <- ts(data$temperature, start = c(1850, 1))
model <- lm(y ~ time(y))
grf <- autoplot(ts(model$residuals, start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("residual")
grf <- grf + xlab("time")
grf <- grf + labs(caption = "(a) LR trend removal") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf + geom_point(size = 0.25, col="black") 
grf <- grf  + font
grfa <- grf

y <- data$temperature
y_transformed <- c(rep(NA, 9), TSPred::mas(y, 10))

grf <- autoplot(ts(y - y_transformed, start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("residual")
grf <- grf + xlab("time")
grf <- grf + labs(caption = "(b) MAS trend removal") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf + geom_point(size = 0.25, col="black") 
grf <- grf  + font
grfb <- grf

y <- c(NA, diff(data$temperature))

grf <- autoplot(ts(y, start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("transformation")
grf <- grf + xlab("time")
grf <- grf + labs(caption = "(c) First order differencing") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf + geom_point(size = 0.25, col="black") 
grf <- grf  + font
grfc <- grf


#PCT
pct_transformed <- TSPred::pct(data$temperature)
yhat <- TSPred::pct.rev(pct_transformed,  attr(pct_transformed, "xi"))
nonstationary.test(pct_transformed)

grf <- autoplot(ts(pct_transformed, start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("transformation")
grf <- grf + xlab("time")
grf <- grf + labs(caption = "(d) PCT") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf + geom_point(size = 0.25, col="black") 
grf <- grf  + font
grfd <- grf

mypng(file="figures/chap2_trend_diff.png", width = 1600, height = 1080) #144 #720*1.5
gridExtra::grid.arrange(grfa, grfb, grfc, grfd,
                        layout_matrix = matrix(c(1,2,3,4), byrow = TRUE, ncol = 2))
dev.off()  

source("header.R")
options(scipen=999)
library(TSPred)


load("data/noaa-global/temp_yearly.RData")
data <- temp_yearly
data$event <- FALSE

y <- data$temperature
yts <- ts(y, start = c(1850, 1))

xts <- time(yts)

yemd <- TSPred::emd(y)
yhat  <- TSPred::emd.rev(yemd)
print(sum(abs(y-yhat)))
autoplot(ts(yhat))
grf <- autoplot(ts(yhat, start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + geom_point(aes(y=yts),size = 0.5, col="black") 
grf <- grf + labs(caption = "(a) EMD with 7 components") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfa <- grf

ex <- yemd
ex[[1]] <- rep(0, length(ex[[1]]))
ex[[2]] <- rep(0, length(ex[[2]]))
ex[[3]] <- rep(0, length(ex[[3]]))
ex[[4]] <- rep(0, length(ex[[4]]))
ex[[5]] <- rep(0, length(ex[[5]]))
yhat  <- TSPred::emd.rev(ex)
print(sum(abs(y-yhat)))
yhattrend <- yhat

grf <- autoplot(yts)
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + geom_point(aes(y=yts),size = 0.5, col="black") 
grf <- grf + geom_line(aes(y=ts(yhattrend, start = c(1850, 1))), linetype = "dashed", col="darkblue") 
grf <- grf + labs(caption = "(b) last IMF + residual") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfb <- grf


ex <- yemd
ex[[7]] <- rep(0, length(ex[[7]]))
ex[[6]] <- rep(0, length(ex[[6]]))
yhat  <- emd.rev(ex)
print(sum(abs(y-yhat)))

grf <- autoplot(ts(yhat, start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("residual")
grf <- grf + xlab("time")
grf <- grf + labs(caption = "(c) first five IMF") 
grf <- grf + geom_point(size = 0.5, col="black") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfc <- grf

mypng(file="figures/chap2_emd.png", width = 1280, height = 1260) 
gridExtra::grid.arrange(grfa, grfb, grfc, 
                        layout_matrix = matrix(c(1,2,3), byrow = TRUE, ncol = 1))
dev.off() 

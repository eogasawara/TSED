source("header.R")

#load("data/noaa-global/temp_monthly.RData")
#data <- temp_monthly
load("data/noaa-global/temp_yearly.RData")
data <- temp_yearly
data$event <- FALSE

y <- data$temperature


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
grf <- grf + fontstyle + font
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
grf <- grf + fontstyle + font
grfb <- grf

mypng(file="figures/chap2_vs.png", width = 1600, height = 720) # 1280 * 1.5
gridExtra::grid.arrange(grfa, grfb, layout_matrix = matrix(c(1,2), byrow = TRUE, ncol = 2))
dev.off() 



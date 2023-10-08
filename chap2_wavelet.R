source("header.R")
options(scipen=999)

load("data/noaa-global/temp_yearly.RData")
data <- temp_yearly
data$event <- FALSE

y <- data$temperature
yts <- ts(y, start = c(1850, 1))
xts <- time(yts)

#ts_data <- arima.sim(list(order=c(1,0,0), ar=0.8), n=100)+10
#y <- ts_data
#yts <- ts(y)
#autoplot(ts(y))

#decompose the time series by maximal overlap discrete wavelet transform
wavelet_components <- WaveletT(y, filter="haar") #"haar", "d4", "la8", "bl14", "c6"
#get wavelet object (necessary for reverse transforming)
wt <- attr(wavelet_components,"wt_obj")
yhat <- WaveletT.rev(pred=NULL, wt)

grf <- autoplot(ts(yhat, start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + geom_point(aes(y=yts),size = 0.5, col="black") 
grf <- grf + labs(caption = "(a) using all \u03b6 and \u03c8 components") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf + fontstyle + font
grfa <- grf

tolerance <- 30

wt <- attr(wavelet_components,"wt_obj")
n <- length(wt@V)
for (i in 1:length(wt@W)) {
  wt@W[[i]] <- as.matrix(rep(0, length(wt@W[[i]])), ncol=1)
}
yhatV <- WaveletT.rev(pred=NULL, wt)
print(sum(abs(y-yhatV)))

grf <- autoplot(yts)
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + geom_line(aes(y=ts(yhatV, start = c(1850, 1))), linetype = "dashed", col="darkblue") 
grf <- grf + geom_vline(xintercept = xts[tolerance], col="black", size = 0.5, linetype="dotted")
grf <- grf + geom_vline(xintercept = xts[length(xts)-tolerance], col="black", size = 0.5, linetype="dotted")
grf <- grf + geom_point(aes(y=yts),size = 0.5, col="black") 
if (n > 1) {
  grf <- grf + labs(caption = sprintf("(b) %d\u03b6 components", n)) 
} else {
  grf <- grf + labs(caption = "(b) \u03b6 component") 
}
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf + fontstyle + font
grfb <- grf
plot(grfb)

wt <- attr(wavelet_components,"wt_obj")
n <- length(wt@W)
for (i in 1:length(wt@V)) {
  wt@V[[i]] <- as.matrix(rep(0, length(wt@V[[i]])), ncol=1)
}
yhatW <- WaveletT.rev(pred=NULL, wt)
print(sum(abs(y-yhatW)))

grf <- autoplot(ts(yhatW, start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + geom_vline(xintercept = xts[tolerance], col="black", size = 0.5, linetype="dotted")
grf <- grf + geom_vline(xintercept = xts[length(xts)-tolerance], col="black", size = 0.5, linetype="dotted")
grf <- grf + ylab("residual")
grf <- grf + xlab("time")
if (n > 1) {
  grf <- grf + labs(caption = sprintf("(c) %d\u03c8 components", n)) 
} else {
  grf <- grf + labs(caption = "(c) \u03c8 component") 
}
grf <- grf + geom_point(size = 0.5, col="black") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf + fontstyle + font
grfc <- grf


mypng(file="figures/chap2_wavelet.png", width = 1280, height = 1260) 
gridExtra::grid.arrange(grfa, grfb,  grfc,
                        layout_matrix = matrix(c(1,2,3), byrow = TRUE, ncol = 1))
dev.off() 

n.ahead <- 5
components_pred <- lapply(wavelet_components, function(comp) forecast(auto.arima(comp), h=n.ahead)$mean)
ts_pred <- WaveletT.rev(components_pred, wt)

ny <- c(y, ts_pred)
autoplot(ts(ny))


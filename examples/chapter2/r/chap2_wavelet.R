library(ggplot2)
library(RColorBrewer)
library(harbinger)
library(forecast)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

# Shared helpers (themes, saving utilities, etc.)
options(scipen = 999)

data(examples_harbinger)

data <- examples_harbinger$global_temperature_yearly
data$event <- FALSE
y <- data$serie
yts <- ts(y, start = c(1850, 1))
xts <- time(yts)
wt <- wavelets::modwt(yts, filter="haar", boundary="periodic")
V <- as.data.frame(wt@V)
W <- as.data.frame(wt@W)
#for (i in 1:length(wt@V)) {
#  wt@V[[i]] <- as.matrix(rep(0, length(wt@V[[i]])), ncol=1)
#}
#iwt <- wavelets::imodwt(wt)
yhat <- apply(V, 1, mean)
residual <- -apply(W, 1, mean)

grf <- autoplot(ts(yts, start = c(1850, 1)))

grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + geom_point(aes(y=yts),size = 0.5, col="black") 
grf <- grf + geom_line(aes(y=yhat), linetype = "dashed", col="darkblue") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfa <- grf

plot(grf)

grf <- autoplot(ts(residual, start = c(1850, 1)))

grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("residual")
grf <- grf + xlab("time")
grf <- grf + geom_point(size = 0.5, col="black") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfb <- grf

plot(grf)

mypng(file = "figures/chap2_wavelet.png", width = 1280, height = 1080)

gridExtra::grid.arrange(grfa, grfb,
                        layout_matrix = matrix(c(1, 2), byrow = TRUE, ncol = 1))
dev.off()

wt <- wavelets::modwt(yts, filter="haar", boundary="periodic")
V <- as.data.frame(wt@V)
W <- as.data.frame(wt@W)
n.ahead <- 5
V_pred <- lapply(V, function(comp) forecast(auto.arima(comp), h=n.ahead)$mean)
W_pred <- lapply(W, function(comp) forecast(auto.arima(comp), h=n.ahead)$mean)
for (i in 1:wt@level) {
  wt@W[[i]] <- as.matrix(c(wt@W[[i]],W_pred[[i]]))
  wt@V[[i]] <- as.matrix(c(wt@V[[i]],V_pred[[i]]))
}
newseries <- c(wt@series,rep(NA,n.ahead))
wt@series <- as.matrix(newseries)
wt@attr.X <- attributes(stats::ts(newseries))
iwt <- wavelets::imodwt(wt)
#gets prediction time series
pred <- stats::ts(utils::tail(iwt,n.ahead),start=(length(iwt)-n.ahead+1))
ny <- c(yts, pred)

autoplot(ts(ny))

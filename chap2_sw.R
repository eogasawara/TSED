source("header.R")
options(scipen=999)
library(ggpmisc)

load("data/noaa-global/temp_yearly.RData")
data <- temp_yearly

data <- data[(nrow(data)-23+1):nrow(data),]

y <- data$temperature
n <- length(y)

obj <- smoothing_freq(n = 9)  
obj <- fit(obj, y)
yl <- transform(obj, y)
yl <- as.integer(names(yl))

colorsg <- brewer.pal(9, 'Greys')

colors <- colorsg[yl]
bcolors <- rep("black", n)

ats <- function(y) {
  yts <- ts(y, frequency=1, start = c(2000, 1))
  return(yts)  
}

sw_size <- 3
xw <- ts_data(y, sw_size)
xw <- round(xw, 2)
xwt <- as.data.frame(xw)
xwt$sw <- 1:nrow(xwt)
xwt <- xwt |> dplyr::select(sw, t2, t1, t0)
xwt <- head(xwt, 5)
xwt$sw <- as.character(xwt$sw)
xwt$t2 <- as.character(xwt$t2)
xwt$t1 <- as.character(xwt$t1)
xwt$t0 <- as.character(xwt$t0)
xwt <- rbind(xwt, data.frame(sw = '...', t2 = '...', t1 = '...', t0 = '...'))
colnames(xwt) <- c('sw', 't-2', 't-1', 't')
xwt <- (xwt)

colors <- colorsg[yl]
bcolors <- rep("black", n)
yb <- ats(rep(14.25, n))

ysw1 <- ats(rep(14.15, n))
ysw1[1:(sw_size-1)] <- NA
ysw2 <- ysw1 - 0.025
ysw3 <- ysw2 - 0.025

colors1 <- c(rep(NA, 2), colors[3:n])
colors2 <- c(rep(NA, 2), colors[2:(n-1)])
colors3 <- c(rep(NA, 2), colors[1:(n-2)])

grf <- autoplot(ats(y)) + theme_bw(base_size = 10) + geom_point(size=1)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature")
grf <- grf + xlab("year")
grf <- grf + annotate(geom="table", family = fancy_font,  x = 2000, y = 14.97, label = list(xwt), size = 5)
grf <- grf + annotate(geom="text", family = fancy_font, x=2013, y=14.3, label="colored representatation for the time series", color="black")
grf <- grf + geom_tile(aes(y=yb), height=0.025, width=1, size = 1, fill = colors, color=bcolors, alpha=0.5) 
grf <- grf + annotate(geom="text", family = fancy_font, x=2013, y=14.2, label="colored representatation for the sliding windows of size 3", color="black")
grf <- grf + annotate(geom="text", family = fancy_font, x=2001, y=ysw1[4], label="t", color="black")
grf <- grf + geom_tile(aes(y=ysw1), height=0.025, width=1, size = 1, fill = colors1, color=bcolors, alpha=0.5) 
grf <- grf + annotate(geom="text", family = fancy_font, x=2001, y=ysw2[4], label="t-1", color="black")
grf <- grf + geom_tile(aes(y=ysw2), height=0.025, width=1, size = 1, fill = colors2, color=bcolors, alpha=0.5) 
grf <- grf + annotate(geom="text", family = fancy_font, x=2001, y=ysw3[4], label="t-2", color="black")
grf <- grf + geom_tile(aes(y=ysw3), height=0.025, width=1, size = 1, fill = colors3, color=bcolors, alpha=0.5) 
grf <- grf + fontstyle + font

save_png(grf, "figures/chap2_sw.png", width = 1280, height = 720)



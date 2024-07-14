source("header.R")
library(harbinger)
options(scipen=999)
library(hht)
library(patchwork)

set.seed(100)

data(examples_harbinger)
data <- examples_harbinger$global_temperature_yearly
data$event <- FALSE

y <- data$serie
yts <- ts(y, start = c(1850, 1))

xts <- time(yts)
id <- 1:length(yts)

model <- hht::CEEMD(yts, id, verbose = FALSE, 0.1, 1)

imfs <- model[["imf"]]

grf <- autoplot(yts)
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfts <- grf

grf <- autoplot(ts(imfs[,1], start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("imf 1")
grf <- grf + xlab("time")
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grf1 <- grf

grf <- autoplot(ts(imfs[,2], start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("imf 2")
grf <- grf + xlab("time")
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grf2 <- grf

grf <- autoplot(ts(imfs[,3], start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("imf 3")
grf <- grf + xlab("time")
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grf3 <- grf

grf <- autoplot(ts(imfs[,4], start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("imf 4")
grf <- grf + xlab("time")
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grf4 <- grf

grf <- autoplot(ts(imfs[,5], start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("imf 5")
grf <- grf + xlab("time")
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grf5 <- grf

grf <- autoplot(ts(imfs[,6], start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("imf 6")
grf <- grf + xlab("time")
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grf6 <- grf



grf <- autoplot(ts(model$residue, start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("residual")
grf <- grf + xlab("time")
grf <- grf + geom_point(size = 0.5, col="black") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfr <- grf

grf <- wrap_plots(grfts, grf1, grf2, grf3, grf4, grf5, grf6, grfr, ncol = 3, widths = c(1, 1, 1), heights = c(1, 1, 1))
save_png(grf, "figures/chap2_emd_decomposition.png", width = 1280, height = 1280)


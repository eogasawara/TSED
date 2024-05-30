source("header.R")
library(harbinger)
options(scipen=999)
library(hht)

set.seed(1)

data(examples_harbinger)
data <- examples_harbinger$global_temperature_yearly
data$event <- FALSE

y <- data$serie
yts <- ts(y, start = c(1850, 1))

xts <- time(yts)
id <- 1:length(yts)

model <- hht::CEEMD(yts, id, verbose = FALSE, 0.1, 1)

residual <- apply(model[["imf"]], 1, sum)

yhat <- model$residue

grf <- autoplot(yts)
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + geom_point(aes(y=yts),size = 0.5, col="black") 
grf <- grf + geom_line(aes(y=ts(yhat, start = c(1850, 1))), linetype = "dashed", col="darkblue") 
grf <- grf + labs(caption = "(a) temperature and EMD trend") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfb <- grf

grf <- autoplot(ts(residual, start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("residual")
grf <- grf + xlab("time")
grf <- grf + labs(caption = "(c) residual (sum of IMFs)") 
grf <- grf + geom_point(size = 0.5, col="black") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfc <- grf

mypng(file="figures/chap2_emd.png", width = 1280, height = 1080) 
gridExtra::grid.arrange(grfb, grfc, 
                        layout_matrix = matrix(c(1,2), byrow = TRUE, ncol = 1))
dev.off() 

source("header.R")
options(scipen=999)
library(ggpmisc)

library(daltoolbox)

data(examples_harbinger)
data <- examples_harbinger$global_temperature_yearly

ats <- function(y) {
  yts <- ts(y, frequency=1, start = c(1850, 1))
  return(yts)  
}

y <- ats(data$serie)
x <- time(y)

sw_size <- 5
test_size <- 10

swy <- ts_data(y, sw_size)

grf <- autoplot(y) + theme_bw(base_size = 10) + geom_point(size=1)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature")
grf <- grf + xlab("year")
grf <- grf + geom_vline(xintercept = x[length(x)-test_size], col="black", linewidth = 0.5, linetype="dotted")
grf <- grf + geom_vline(xintercept = x[length(x)-test_size-sw_size+1], col="black", linewidth = 0.5, linetype="dotted")
grf <- grf + labs(caption = "(a)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfS <- grf

### ts_gminmax

preproc <- ts_norm_gminmax(remove_outliers = FALSE)
preproc <- fit(preproc, swy)
nswy <- transform(preproc, swy)

grf <- autoplot(ts(as.vector(nswy[nrow(nswy),]))) + theme_bw(base_size = 10) + geom_point(size=1)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("")
grf <- grf + xlab("global min-max")
grf <- grf + labs(caption = "(b)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grf <- grf + ylim(0, 1)
grfGA <- grf

### ts_diff

preproc <- ts_norm_diff(remove_outliers = FALSE)
preproc <- fit(preproc, swy)
nswy <- transform(preproc, swy)

grf <- autoplot(ts(as.vector(nswy[nrow(nswy),]))) + theme_bw(base_size = 10) + geom_point(size=1)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("")
grf <- grf + xlab("DIF")
grf <- grf + labs(caption = "(c)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grf <- grf + ylim(0, 1)
grfDA <- grf

### ts_swminmax()

preproc <- ts_norm_swminmax(remove_outliers = FALSE)
preproc <- fit(preproc, swy)
nswy <- transform(preproc, swy)

grf <- autoplot(ts(as.vector(nswy[nrow(nswy),]))) + theme_bw(base_size = 10) + geom_point(size=1)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("")
grf <- grf + xlab("sw min-max")
grf <- grf + labs(caption = "(d)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grf <- grf + ylim(0, 1)
grfMA <- grf

### ts_an()

preproc <- ts_norm_an()
preproc <- fit(preproc, swy)
nswy <- transform(preproc, swy)

grf <- autoplot(ts(as.vector(nswy[nrow(nswy),]))) + theme_bw(base_size = 10) + geom_point(size=1)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("")
grf <- grf + xlab("AN")
grf <- grf + labs(caption = "(e)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grf <- grf + ylim(0, 1)
grfAA <- grf

mypng(file="figures/chap2_norm.png", width=1920, height=1080) 
gridExtra::grid.arrange(grfS, grfGA, grfDA, grfMA, grfAA,
                        layout_matrix = matrix(c(1,1,1,1,2,3,4,5), byrow = TRUE, ncol = 4))
dev.off() 



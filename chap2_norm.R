source("header.R")
options(scipen=999)
library(ggpmisc)

library(daltoolbox)
library(harbinger)

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
range <- length(y)-77-1

swy <- ts_data(y, sw_size)

sw_range <- swy[(nrow(swy)-range+1),]


grf <- autoplot(y) + theme_bw(base_size = 10) + geom_point(size=1)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature")
grf <- grf + xlab("year")
grf <- grf + geom_vline(xintercept = x[length(x)-sw_size+1-range+1], col="darkgreen", linewidth = 0.5, linetype="dotted")
grf <- grf + geom_vline(xintercept = x[length(x)-range+1], col="darkgreen", linewidth = 0.5, linetype="dotted")
grf <- grf + labs(caption = "(a)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfS <- grf

### ts_gminmax


gen_graphics <- function(dgminmax, ddiff, dswminmax, d_an) {
  grf <- autoplot(ts(as.vector(dgminmax[nrow(dgminmax),]))) + theme_bw(base_size = 10) + geom_point(size=1)
  grf <- grf + theme(plot.title = element_blank())
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + ylab("")
  grf <- grf + xlab("global min-max")
  grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
  grf <- grf  + font
  grf <- grf + ylim(0, 1)
  grfGA <- grf
  
  
  grf <- autoplot(ts(as.vector(ddiff[nrow(ddiff),]))) + theme_bw(base_size = 10) + geom_point(size=1)
  grf <- grf + theme(plot.title = element_blank())
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + ylab("")
  grf <- grf + xlab("diff with scaling")
  grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
  grf <- grf  + font
  grf <- grf + ylim(0, 1)
  grfDA <- grf
  
  grf <- autoplot(ts(as.vector(dswminmax[nrow(dswminmax),]))) + theme_bw(base_size = 10) + geom_point(size=1)
  grf <- grf + theme(plot.title = element_blank())
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + ylab("")
  grf <- grf + xlab("sw min-max")
  grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
  grf <- grf  + font
  grf <- grf + ylim(0, 1)
  grfMA <- grf

  grf <- autoplot(ts(as.vector(d_an[nrow(d_an),]))) + theme_bw(base_size = 10) + geom_point(size=1)
  grf <- grf + theme(plot.title = element_blank())
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + ylab("")
  grf <- grf + xlab("AN") + theme(axis.title.x = element_text(colour = "darkblue"))
  grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
  grf <- grf  + font
  grf <- grf + ylim(0, 1)
  grfAA <- grf

  return(list(grfGA=grfGA, grfDA=grfDA, grfMA=grfMA, grfAA=grfAA))
}

ngminmax <- ts_norm_gminmax(remove_outliers = FALSE)
ngminmax <- fit(ngminmax, swy)
dgminmax <- transform(ngminmax, sw_range)

### ts_diff

ndiff <- ts_norm_diff(remove_outliers = FALSE)
ndiff <- fit(ndiff, swy)
ddiff <- transform(ndiff, sw_range)

### ts_swminmax()

nswminmax <- ts_norm_swminmax(remove_outliers = FALSE)
nswminmax <- fit(nswminmax, swy)
dswminmax <- transform(nswminmax, sw_range)

### ts_an()

n_an <- ts_norm_an()
n_an <- fit(n_an, swy)
d_an <- transform(n_an, sw_range)

list <- gen_graphics(dgminmax, ddiff, dswminmax, d_an)
list$grfGA <- list$grfGA + labs(caption = "(b)") + theme(plot.caption = element_text(colour = "darkgreen"))
list$grfDA <- list$grfDA + labs(caption = "(c)") + theme(plot.caption = element_text(colour = "darkgreen"))
list$grfMA <- list$grfMA + labs(caption = "(d)") + theme(plot.caption = element_text(colour = "darkgreen"))
list$grfAA <- list$grfAA + labs(caption = "(e)") + theme(plot.caption = element_text(colour = "darkgreen"))


mypng(file="figures/chap2_norm.png", width=1920, height=1080) 
gridExtra::grid.arrange(grfS, list$grfGA, list$grfDA, list$grfMA, list$grfAA,
                        layout_matrix = matrix(c(1,1,1,1,2,3,4,5), byrow = TRUE, ncol = 4))
dev.off() 



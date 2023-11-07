library(RColorBrewer)
library(ggplot2)
library(ragg)

mypng <- function(file, width, height) {
  return(agg_png(file, width = width, height = height, res = 144)) 
}

mypdf <- function(file, width, height) {
  return(pdf(file, width = width/100, height = height/100))
}

save_png <- function(grf, filename, width, height) {
  mypng(filename, width = width, height = height) 
  plot(grf)
  dev.off() 
}

save_pdf <- function(grf, filename, width, height) {
  mypdf(filename, width = width, height = height)
  plot(grf)
  dev.off() 
}

colors <- brewer.pal(9, 'Set1')[c(1:5,7:9)]
font <- theme(text = element_text(size=16))

prepare_grf <- function(grf, xlabel) {
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(plot.title = element_blank())
  grf <- grf + theme(panel.grid.major = element_blank())
  grf <- grf + theme(panel.grid.minor = element_blank())
  grf <- grf + ylab(xlabel)
  grf <- grf + xlab("time")  
  grf <- grf 
  grf <- grf + font
  return(grf)
}


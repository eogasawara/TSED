library(tsmp)

source("header.R")
options(scipen=999)
library(ggpmisc)

binning_sax <- function(v, a) {
  p <- seq(from = 0, to = 1, by = 1/a)
  q <- stats::quantile(v, p)
  qf <- matrix(c(q[1:(length(q)-1)],q[2:(length(q))]), ncol=2)
  vp <- cut(v, unique(q), FALSE, include.lowest=TRUE)
  m <- tapply(v, vp, mean)
  vm <- m[vp]
  mse <- mean((v - vm)^2, na.rm = TRUE)
  return (list(binning=m, bins_factor=vp, q=q, qf=qf, bins=vm, mse=mse))
}

norm_data <- function(data, x) {
  data <- data.frame(temperature = data)
  norm <- zscore()
  norm <- fit(norm, data)
  data <- transform(norm, data)
  data$x <- x
  return(data)
}

load("data/noaa-global/temp_monthly.RData")
ts_data_m <- ts(temp_monthly$temperature, frequency=12, start = c(1850, 1))

load("data/noaa-global/temp_yearly.RData")
ts_data <- ts(temp_yearly$temperature, frequency=1, start = c(1850, 1))

data_m <- norm_data(ts_data_m, temp_monthly$x)
mybin_m <- binning_sax(data_m$temperature, 3)

grf <- plot_ts(x = data_m$x, y = data_m$temperature) + font
grf <- grf + scale_x_date(breaks = "10 years",  date_labels = "%Y",  limits = c(as.Date("1850-01-01"), as.Date("2030-01-01")))
grf <- grf + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
grf <- grf + geom_hline(yintercept = mybin_m$q[2], col="black", size = 0.5, linetype="dashed")
grf <- grf + geom_hline(yintercept = mybin_m$q[3], col="black", size = 0.5, linetype="dashed")
grf <- grf + annotate(geom="text", x=as.Date('2028-01-01'), y=-1.1, label="A", color="black")
grf <- grf + annotate(geom="text", x=as.Date('2028-01-01'), y=-0.25, label="B", color="black")
grf <- grf + annotate(geom="text", x=as.Date('2028-01-01'), y= 0.6, label="C", color="black")
grf <- grf + annotate(geom="text", x=as.Date('1940-01-01'), y=-2, label="(a)", color="black")
grf$layers[[1]]$aes_params$size <- 0.25
grfA <- grf


data <- norm_data(ts_data, temp_yearly$x)
mybin <- binning_sax(data$temperature, 3)

grf <- plot_ts(x = data$x, y = data$temperature) + font
grf <- grf + scale_x_date(breaks = "10 years",  date_labels = "%Y",  limits = c(as.Date("1850-01-01"), as.Date("2030-01-01")))
grf <- grf + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
grf <- grf + geom_hline(yintercept = mybin$q[2], col="black", size = 0.5, linetype="dashed")
grf <- grf + geom_hline(yintercept = mybin$q[3], col="black", size = 0.5, linetype="dashed")
grf <- grf + annotate(geom="text", x=as.Date('2028-01-01'), y=-1.1, label="A", color="black")
grf <- grf + annotate(geom="text", x=as.Date('2028-01-01'), y=-0.25, label="B", color="black")
grf <- grf + annotate(geom="text", x=as.Date('2028-01-01'), y= 0.6, label="C", color="black")
grf <- grf + annotate(geom="text", x=as.Date('1940-01-01'), y=-2, label="(b)", color="black")
grf$layers[[1]]$aes_params$size <- 0.25
grfB <- grf


mypng(file="figures/chap5_preprocessing.png", width = 1280, height = 480) #144 #720*1.75
gridExtra::grid.arrange(grfA, grfB, layout_matrix = matrix(c(1,2), byrow = TRUE, ncol = 2))
dev.off()  

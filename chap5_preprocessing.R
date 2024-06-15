source("header.R")
options(scipen=999)
library(ggpmisc)
library(tsmp)
library(daltoolbox)
library(harbinger)

plot_ts <- function (x = NULL, y, label_x = "", label_y = "", color = "black", size=1) 
{
  y <- as.vector(y)
  if (is.null(x)) 
    x <- 1:length(y)
  grf <- ggplot() + geom_point(aes(x = x, y = y), color = color, size=size) + 
    geom_line(aes(x = x, y = y), color = color)
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank())
  grf <- grf + theme(legend.title = element_blank()) + theme(legend.position = "bottom") + 
    theme(legend.key = element_blank())
  return(grf)
}


paa <- function(v, n) {
  data <- ts_data(v, n)
  vx <- apply(data, 1, mean, na.rm=TRUE)
  pos <- (1:length(vx) %% n)
  vx <- vx[pos == pos[1]]
  return(vx)
}

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
  data <- data.frame(serie = data)
  norm <- zscore()
  norm <- fit(norm, data)
  data <- transform(norm, data)
  data$x <- x
  return(data)
}

data(examples_motifs)
data <- examples_motifs$mitdb102

data_n <- paa(data$serie, 1)
i_n <- 1:length(data_n)


data_n_n <- norm_data(data_n, i_n)

mybin_n_n <- binning_sax(data_n_n$serie, 5)

data_paa <- paa(data$serie, 20)
i_paa <- 1:length(data_paa)

data_paa_n <- norm_data(data_paa, i_paa)

mybin_paa_n <- binning_sax(data_paa_n$serie, 5)

grf <- plot_ts(x = data_n_n$x, y = data_n_n$serie, size=0.5) + font
grf <- grf + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
grf <- grf + annotate(geom="text", x=-200, y=(mybin_n_n$q[1]+mybin_n_n$q[2])/2, label=" ", color="black")
grf <- grf + annotate(geom="text", x=-400, y=(mybin_n_n$q[2]+mybin_n_n$q[3])/2, label=" ", color="black")
grf <- grf + annotate(geom="text", x=-200, y=(mybin_n_n$q[3]+mybin_n_n$q[4])/2, label=" ", color="black")
grf <- grf + annotate(geom="text", x=-400, y=(mybin_n_n$q[4]+mybin_n_n$q[5])/2, label=" ", color="black")
grf <- grf + annotate(geom="text", x=-200, y=(mybin_n_n$q[5]+mybin_n_n$q[6])/2, label=" ", color="black")
grfA <- grf
plot(grfA)


grf <- plot_ts(x = data_paa_n$x, y = data_paa_n$serie, size=0.5) + font
grf <- grf + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
grf <- grf + geom_hline(yintercept = mybin_paa_n$q[1], col="black", size = 0.5, linetype="dashed")
grf <- grf + geom_hline(yintercept = mybin_paa_n$q[2], col="black", size = 0.5, linetype="dashed")
grf <- grf + geom_hline(yintercept = mybin_paa_n$q[3], col="black", size = 0.5, linetype="dashed")
grf <- grf + geom_hline(yintercept = mybin_paa_n$q[4], col="black", size = 0.5, linetype="dashed")
grf <- grf + geom_hline(yintercept = mybin_paa_n$q[5], col="black", size = 0.5, linetype="dashed")
grf <- grf + annotate(geom="text", x=-10, y=(mybin_paa_n$q[1]+mybin_paa_n$q[2])/2, label="A", color="black")
grf <- grf + annotate(geom="text", x=-20, y=(mybin_paa_n$q[2]+mybin_paa_n$q[3])/2, label="B", color="black")
grf <- grf + annotate(geom="text", x=-10, y=(mybin_paa_n$q[3]+mybin_paa_n$q[4])/2, label="C", color="black")
grf <- grf + annotate(geom="text", x=-20, y=(mybin_paa_n$q[4]+mybin_paa_n$q[5])/2, label="D", color="black")
grf <- grf + annotate(geom="text", x=-10, y=(mybin_paa_n$q[5]+mybin_paa_n$q[6])/2, label="E", color="black")
grfB <- grf
plot(grfB)

mypng(file="figures/chap5_preprocessing.png", width = 1280, height = 1260) #144 #720*1.75
gridExtra::grid.arrange(grfA, grfB, layout_matrix = matrix(c(1,2), byrow = TRUE, ncol = 1))
dev.off()  

library(ggplot2)
library(RColorBrewer)
library(ggpmisc)
library(tsmp)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

library(ggplot2)
library(RColorBrewer)
library(ggpmisc)
library(tsmp)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

options(scipen=999)


# Simple point+line plot helper for time series
plot_ts <- function (x = NULL, y, label_x = "", label_y = "", color = "black", size = 1) {
  y <- as.vector(y)
  if (is.null(x)) x <- seq_along(y)
  grf <- ggplot() +
    geom_point(aes(x = x, y = y), color = color, size = size) +
    geom_line(aes(x = x, y = y), color = color) +
    xlab(label_x) + ylab(label_y) +
    theme_bw(base_size = 10) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.title = element_blank(), legend.position = "bottom", legend.key = element_blank())
  return(grf)
}
# Piecewise Aggregate Approximation (average over blocks of size n)
paa <- function(v, n) {
  data <- ts_data(v, n)
  vx <- apply(data, 1, mean, na.rm = TRUE)
  pos <- (seq_along(vx) %% n)
  vx <- vx[pos == pos[1]]
  return(vx)
}
# Quantile-based binning (SAX-style) with alphabet size a
binning_sax <- function(v, a) {
  p <- seq(from = 0, to = 1, by = 1 / a)
  q <- stats::quantile(v, p)
  qf <- matrix(c(q[1:(length(q) - 1)], q[2:length(q)]), ncol = 2)
  vp <- cut(v, unique(q), labels = FALSE, include.lowest = TRUE)
  m <- tapply(v, vp, mean)
  vm <- m[vp]
  mse <- mean((v - vm)^2, na.rm = TRUE)
  return(list(binning = m, bins_factor = vp, q = q, qf = qf, bins = vm, mse = mse))
}
# Z-score normalization helper that preserves an index column
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
# No compression (block size 1) and normalization
data_n <- paa(data$serie, 1)
i_n <- seq_along(data_n)
data_n_n <- norm_data(data_n, i_n)
mybin_n_n <- binning_sax(data_n_n$serie, 5)
# PAA compression (block size 20), normalization and binning
data_paa   <- paa(data$serie, 20)
i_paa      <- seq_along(data_paa)
data_paa_n <- norm_data(data_paa, i_paa)
mybin_paa_n <- binning_sax(data_paa_n$serie, 5)
# Plot A: normalized series with implicit quantile regions (placeholders)
grf <- plot_ts(x = data_n_n$x, y = data_n_n$serie, size = 0.5) + font
grf <- grf + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
grf <- grf + ggplot2::annotate(geom = "text", x = -200, y = (mybin_n_n$q[1] + mybin_n_n$q[2]) / 2, label = " ", color = "black")
grf <- grf + ggplot2::annotate(geom = "text", x = -400, y = (mybin_n_n$q[2] + mybin_n_n$q[3]) / 2, label = " ", color = "black")
grf <- grf + ggplot2::annotate(geom = "text", x = -200, y = (mybin_n_n$q[3] + mybin_n_n$q[4]) / 2, label = " ", color = "black")
grf <- grf + ggplot2::annotate(geom = "text", x = -400, y = (mybin_n_n$q[4] + mybin_n_n$q[5]) / 2, label = " ", color = "black")
grf <- grf + ggplot2::annotate(geom = "text", x = -200, y = (mybin_n_n$q[5] + mybin_n_n$q[6]) / 2, label = " ", color = "black")
grfA <- grf

grfA

grf <- plot_ts(x = data_paa_n$x, y = data_paa_n$serie, size = 0.5) + font
grf <- grf + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
grf <- grf + geom_hline(yintercept = mybin_paa_n$q[1], col = "black", linewidth = 0.5, linetype = "dashed")
grf <- grf + geom_hline(yintercept = mybin_paa_n$q[2], col = "black", linewidth = 0.5, linetype = "dashed")
grf <- grf + geom_hline(yintercept = mybin_paa_n$q[3], col = "black", linewidth = 0.5, linetype = "dashed")
grf <- grf + geom_hline(yintercept = mybin_paa_n$q[4], col = "black", linewidth = 0.5, linetype = "dashed")
grf <- grf + geom_hline(yintercept = mybin_paa_n$q[5], col = "black", linewidth = 0.5, linetype = "dashed")
grf <- grf + ggplot2::annotate(geom = "text", x = -10, y = (mybin_paa_n$q[1] + mybin_paa_n$q[2]) / 2, label = "A", color = "black")
grf <- grf + ggplot2::annotate(geom = "text", x = -20, y = (mybin_paa_n$q[2] + mybin_paa_n$q[3]) / 2, label = "B", color = "black")
grf <- grf + ggplot2::annotate(geom = "text", x = -10, y = (mybin_paa_n$q[3] + mybin_paa_n$q[4]) / 2, label = "C", color = "black")
grf <- grf + ggplot2::annotate(geom = "text", x = -20, y = (mybin_paa_n$q[4] + mybin_paa_n$q[5]) / 2, label = "D", color = "black")
grf <- grf + ggplot2::annotate(geom = "text", x = -10, y = (mybin_paa_n$q[5] + mybin_paa_n$q[6]) / 2, label = "E", color = "black")
grfB <- grf

grfB

#mypng(file = "figures/chap5_preprocessing.png", width = 1280, height = 1260)
gridExtra::grid.arrange(grfA, grfB, layout_matrix = matrix(c(1, 2), byrow = TRUE, ncol = 1))
#dev.off()

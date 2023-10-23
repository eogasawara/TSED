source("header.R")
library(readxl)
library(daltoolbox)
library(dplyr)
library(stats)

hcp_binseg <- function(Q = 2) {
  obj <- harbinger()
  obj$Q <- Q
  class(obj) <- append("hcp_binseg", class(obj))
  return(obj)
}

detect.hcp_binseg <- function(obj, serie, ...) {
  library(changepoint)
  if(is.null(serie)) stop("No data was provided for computation", call. = FALSE)
  
  non_na <- which(!is.na(serie))
  data <- serie[non_na]
  
  # Perform change point detection using Binary Segmentation
  cpt_result <- cpt.meanvar(data, method = "BinSeg", Q = obj$Q)
  #plot(cpt_result, type = "l")
  #Binary Segmentation: Scott, A. J. and Knott, M. (1974) A Cluster Analysis Method for Grouping Means in the Analysis of Variance, Biometrics 30(3), 507–512
  
  
  inon_na <- rep(FALSE, length(non_na))
  n <- length(cpt_result@cpts)
  if (n > 1)
    inon_na[cpt_result@cpts[1:(n-1)]] <- TRUE
  
  i <- rep(NA, length(serie))
  i[non_na] <- inon_na
  
  detection <- data.frame(idx=1:length(serie), event = i, type="")
  detection$type[i] <- "changepoint"
  
  return(detection)
}

n <- 100  # Number of time points
data <- c(sin((1:n)/pi), 2*sin((1:n)/pi), 10 + sin((1:n)/pi), 10-10/n*(1:n)+sin((1:n)/pi)/2, sin((1:n)/pi)/2)
event <- rep(FALSE, n)

model <- fit(hcp_binseg(Q = 10), data)
detection <- detect(model, data)

grf <- har_plot(model, data, detection)
grf <- grf + ylab("value")
grf <- grf + font

save_png(grf, "figures/chap4_binseg.png", 1280, 720)

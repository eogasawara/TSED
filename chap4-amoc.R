source("header.R")
library(readxl)
library(daltoolbox)
library(dplyr)
library(stats)

hcp_amoc <- function() {
  obj <- harbinger()
  class(obj) <- append("hcp_amoc", class(obj))
  return(obj)
}

detect.hcp_amoc <- function(obj, serie, ...) {
  library(changepoint)
  if(is.null(serie)) stop("No data was provided for computation", call. = FALSE)
  
  non_na <- which(!is.na(serie))
  data <- serie[non_na]
  
  # Perform change point detection using AMOC
  #cpt_result <- cpt.mean(data, penalty="None", method="AMOC",test.stat="CUSUM") 
  #plot(cpt_result, type = "l")
  cpt_result <- cpt.meanvar(data, method = "AMOC", penalty="MBIC", test.stat="Normal")
  #plot(cpt)
  #Hinkley, D. V. Inference about the change-point in a sequence of random variables. Biometrika, 57(1):1–17, 1970
  
  
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

set.seed(1)
n <- 100  # Number of time points
data <- c(sin((1:n)/pi), 2*sin((1:n)/pi), 10 + sin((1:n)/pi), 10-10/n*(1:n)+sin((1:n)/pi)/2, sin((1:n)/pi)/2)
event <- rep(FALSE, n)

model <- fit(hcp_amoc(), data)
detection <- detect(model, data)
idx <- detection$idx[detection$event == TRUE]

grf <- har_plot(model, data, detection)
grf <- grf + ylab("value")
grf <- grf + font

save_png(grf, "figures/chap4_amoc.png", 1280, 720)

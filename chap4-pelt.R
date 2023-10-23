source("header.R")
library(readxl)
library(daltoolbox)
library(dplyr)
library(stats)

hcp_pelt <- function() {
  obj <- harbinger()
  class(obj) <- append("hcp_pelt", class(obj))
  return(obj)
}

detect.hcp_pelt <- function(obj, serie, ...) {
  library(changepoint)
  if(is.null(serie)) stop("No data was provided for computation", call. = FALSE)
  
  non_na <- which(!is.na(serie))
  data <- serie[non_na]
  
  # Perform change point detection using PELT
  #cpt_result <- cpt.meanvar(data, method = "PELT", test.stat = "Normal", pen.value = "AIC")
  cpt_result <- cpt.meanvar(data, method = "PELT", test.stat = "Normal", pen.value = "MBIC")
  #plot(cpt)
  #PELT Algorithm: Killick R, Fearnhead P, Eckley IA (2012) Optimal detection of changepoints with a linear computational cost, JASA 107(500), 1590–1598
  
  #plot(cpt_result, type = "l")
  
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

model <- fit(hcp_pelt(), data)
detection <- detect(model, data)

grf <- har_plot(model, data, detection)
grf <- grf + ylab("value")
grf <- grf + font


save_png(grf, "figures/chap4_pelt.png", 1280, 720)



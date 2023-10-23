source("header.R")
library(readxl)
library(daltoolbox)
library(dplyr)
library(stats)

#generalized fluctuation test
#A. Zeileis, C. Kleiber, W. Krämer, and K. Hornik, 2003, Testing and dating of structural changes in practice, Computational Statistics & Data Analysis, v. 44, n. 1 (Oct.), p. 109–123. 
#A. Zeileis, F. Leisch, K. Hornik, and C. Kleiber 2002.  Strucchange: An R package for testing for structural change in linear regression models.

hcp_gft <- function() {
  obj <- harbinger()
  class(obj) <- append("hcp_gft", class(obj))
  return(obj)
}

detect.hcp_gft <- function(obj, serie, ...) {
  library(strucchange)
  if(is.null(serie)) stop("No data was provided for computation", call. = FALSE)
  
  non_na <- which(!is.na(serie))
  data <- serie[non_na]
  x <- 1:length(data)
  
  breaks <- breakpoints(data ~ x)
  #plot(ewma ~ x, main = "ewma")
  #lines(x, fitted(model), col = "red", lwd = 2)
  #cat("Change point(s) detected at:", breakpoints(model), "\n")  
  
  inon_na <- rep(FALSE, length(non_na))
  n <- length(breaks$breakpoints)
  if (n > 0)
    inon_na[breaks$breakpoints] <- TRUE
  
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

model <- fit(hcp_gft(), data)
detection <- detect(model, data)

grf <- har_plot(model, data, detection)
grf <- grf + ylab("value")
grf <- grf + font

save_png(grf, "figures/chap4_gft.png", 1280, 720)

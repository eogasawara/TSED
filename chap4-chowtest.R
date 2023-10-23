source("header.R")
library(readxl)
library(daltoolbox)
library(dplyr)
library(stats)

#chow test / f test
#A. Zeileis, C. Kleiber, W. Krämer, and K. Hornik, 2003, Testing and dating of structural changes in practice, Computational Statistics & Data Analysis, v. 44, n. 1 (Oct.), p. 109–123. 
#A. Zeileis, F. Leisch, K. Hornik, and C. Kleiber 2002.  Strucchange: An R package for testing for structural change in linear regression models.

hcp_chow <- function() {
  obj <- harbinger()
  class(obj) <- append("hcp_chow", class(obj))
  return(obj)
}

detect.hcp_chow <- function(obj, serie, ...) {
  library(strucchange)
  if(is.null(serie)) stop("No data was provided for computation", call. = FALSE)
  
  non_na <- which(!is.na(serie))
  data <- serie[non_na]
  
  y <- data
  x <- 1:length(y)
  
  # Perform using f-test
  model <- Fstats(y ~ x)
  breaks <- breakpoints(model)

  #plot(y ~ x, main = "Chow test")
  #abline(v=breaks$breakpoints, col="blue")  
  
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

model <- fit(hcp_chow(), data)
detection <- detect(model, data)

grf <- har_plot(model, data, detection)
grf <- grf + ylab("value")
grf <- grf + font


save_png(grf, "figures/chap4_chow.png", 1280, 720)

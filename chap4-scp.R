source("header.R")

hcp_scp <- function(sw_size = 30) {
  obj <- harbinger()
  obj$sw_size <- sw_size
  
  class(obj) <- append("hcp_scp", class(obj))
  return(obj)
}

detect.hcp_scp <- function(obj, serie, ...) {
  analyze_window <- function(data, offset) {
    n <- length(data)
    y <- data.frame(t = 1:n, y = data)
    
    mdl <- stats::lm(y~t, y)
    err <- mean(mdl$residuals^2)
    
    y_a <- y[1:(offset-1),]
    mdl_a <- stats::lm(y~t, y_a)
    y_d <- y[(offset+1):n,]
    mdl_d <- stats::lm(y~t, y_d)
    
    err_ad <- mean(obj$har_residuals(c(mdl_a$residuals,mdl_d$residuals)))
    
    #return 1-error on whole window; 2-error on window halves; 3-error difference
    return(data.frame(mdl=err, mdl_ad=err_ad, mdl_dif=err-err_ad))
  }
  
  if(is.null(serie)) stop("No data was provided for computation", call. = FALSE)
  
  non_na <- which(!is.na(serie))
  
  sx <- ts_data(stats::na.omit(serie), obj$sw_size)
  obj$offset <- round(obj$sw_size/2)
  
  #===== Analyzing all data windows ======
  errors <- do.call(rbind,apply(sx, 1, analyze_window, obj$offset))
  
  #Returns index of windows with outlier error differences
  s <- errors$mdl_dif
  outliers <- obj$har_outliers_idx(s)
  outliers <- obj$har_outliers_group(outliers, length(s))
  
  index.cp <- c(rep(FALSE, obj$offset-1), outliers, rep(FALSE, obj$sw_size-obj$offset))
  
  inon_na <- index.cp
  
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

model <- fit(hcp_scp(sw_size = 30), data)
detection <- detect(model, data)

grf <- har_plot(model, data, detection)
grf <- grf + ylab("value")
grf <- grf + font

save_png(grf, "figures/chap4_scp.png", 1280, 720)

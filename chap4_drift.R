source("header.R")
options(scipen=999)
library(ggpmisc)
library(daltoolbox)
library(harbinger)
library(heimdall)
library(patchwork)

#'@title ADWIN method
#'@description Adaptive Windowing method for concept drift detection <doi:10.1137/1.9781611972771.42>.
#'@param target_feat Feature to be monitored.
#'@param delta The significance parameter for the ADWIN algorithm.
#ADWIN detection: Bifet, Albert, and Ricard Gavalda. “Learning from time-changing data with adaptive windowing.” In Proceedings of the 2007 SIAM international conference on data mining, pp. 443-448. Society for Industrial and Applied Mathematics, 2007.
#'@return `dfr_adwin` object
#'@examples
#'#Use the same example of dfr_cumsum changing the constructor to:
#'#model <- dfr_adwin(target_feat='serie')
#'@import reticulate
#'@export
dfr_adwin <- function(target_feat, delta=0.002) {
  obj <- dist_based(target_feat=target_feat)
  
  # Attributes
  state <- list()
  
  state$delta <- delta
  reticulate::source_python("https://raw.githubusercontent.com/cefet-rj-dal/heimdall/main/inst/python/adwin.py")
  state$adwin <- ADWIN(
    delta=delta
  )
  
  obj$drifted <- FALSE
  obj$state <- state
  class(obj) <- append("dfr_adwin", class(obj))
  return(obj)
}

#'@export
update_state.dfr_adwin <- function(obj, value){
  
  state <- obj$state
  
  state$adwin$add_element(value)
  
  obj$state <- state
  has_drift <- state$adwin$detected_change()
  if (has_drift){
    obj$drifted <- has_drift
    return(list(obj=obj, pred=obj$drifted))
  }
  else{
    return(list(obj=obj, pred=FALSE))
  }
}

#'@export
fit.dfr_adwin <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_adwin <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_adwin(
    target_feat = obj$target_feat,
    delta=obj$state$delta
  )$state
  return(obj) 
}


event_plot <- function(model, serie, event, prediction, title) {
  
  detection <- NULL
  output <- list(obj=model, pred=FALSE)
  for (i in 1:length(serie)){
    output <- update_state(output$obj, prediction[i])
    if (output$pred){
      type <- 'changepoint'
      output$obj <- reset_state(output$obj)
    }else{
      type <- ''
    }
    detection <- rbind(detection, data.frame(idx=i, event=output$pred, type=type))
  }
  
  dataset  <- data.frame(col_TP_verde = logical(length(dataset$serie)), col_FN_blue = logical(length(dataset$serie)), col_FP_red = logical(length(dataset$serie)))
  
  dataset$col_TP_verde <- as.logical(event) & as.logical(detection$event)
  dataset$col_FN_blue <- as.logical(event) & as.logical(!detection$event)
  dataset$col_FP_red <- (!as.logical(event)) & as.logical(detection$event)
  
  dataset$x <- 1:length(serie)
  dataset$y <- 0  

  grf <-  ggplot() + geom_line(data = dataset, aes(x = x, y = y), color = "black") 
  grf <- grf + geom_point(data = subset(dataset, col_TP_verde == TRUE), size = 2, col = "green", aes(x = x, y = y))
  grf <- grf + geom_point(data = subset(dataset, col_FN_blue == TRUE), size = 2, col = "blue", aes(x = x, y = y))
  grf <- grf + geom_point(data = subset(dataset, col_FP_red == TRUE), size = 2, col = "red", aes(x = x, y = y))
  grf <- grf + theme_minimal()
  grf <- grf + theme(
    panel.background = element_rect(fill = "white"),  # Define a cor de fundo como branco
    panel.grid.major = element_blank(),  # Remove linhas de grade principais
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank() 
  )
  grf <- grf+ ylab(title) + xlab(NULL)   
  return(grf)
}


data(examples_changepoints)
dataset <- examples_changepoints$complex

#### Time Series

dataset$x <- 1:length(dataset$serie)
dataset$prediction <- dataset$serie > 4

grf_base <- ggplot(data = dataset, aes(x = 1:length(serie), y = serie)) 
grf_base <- grf_base + geom_line() 
grf_base <- grf_base + geom_point(color = "black", size = 0.5) 
grf_base <- grf_base + geom_point(data = subset(dataset, event == TRUE),  aes(x = x), color = "blue", size = 1.5)   # Adiciona pontos azuis onde dataset$event = TRUE
grf_base <- grf_base + theme_minimal() 
grf_base <- grf_base +theme(
  panel.background = element_rect(fill = "white"), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank()   
)
grf_base <- grf_base + labs( x = "time", y = "values") 

############# Models ################

model <- dfr_ddm()
grf_ddm <- event_plot(model, dataset$serie, dataset$event, dataset$prediction, "DDM")

model <- dfr_eddm()
grf_eddm <- event_plot(model, dataset$serie, dataset$event, dataset$prediction, "EDDM")

model <- dfr_hddm()
grf_hddm <- event_plot(model, dataset$serie, dataset$event, dataset$prediction, "HDDM")

model <- dfr_cumsum(lambda = 100)
grf_cumsum <- event_plot(model, dataset$serie, dataset$event, dataset$prediction, "CUMSUM")

model <- dfr_ecdd(lambda = 0.2, min_run_instances = 50, average_run_length = 100)
grf_ecdd <- event_plot(model, dataset$serie, dataset$event, dataset$prediction, "ECDD")

model <- dfr_mcdd(target_feat = 'serie', alpha = 0.05, window_size = 100)
grf_mcdd <- event_plot(model, dataset$serie, dataset$event, dataset$serie, "MCDD")

model <- dfr_kldist(target_feat = 'serie')
grf_kldist <- event_plot(model, dataset$serie, dataset$event, dataset$serie, "KLD")

model <- dfr_kswin(target_feat = 'serie')
grf_kswin <- event_plot(model, dataset$serie, dataset$event, dataset$serie, "KSWIN")

model <- dfr_page_hinkley(target_feat = 'serie')
grf_page_hinkley <- event_plot(model, dataset$serie, dataset$event, dataset$serie, "Page Hinkley")

model <- dfr_adwin(target_feat = 'serie')
grf_adwin <- event_plot(model, dataset$serie, dataset$event, dataset$serie, "ADWIN")


grf <- wrap_plots(grf_base, grf_ddm, grf_eddm, grf_hddm, grf_cumsum, grf_ecdd,
                  grf_mcdd, grf_kldist, grf_kswin, grf_page_hinkley, grf_adwin, 
                                  ncol = 1,   widths = c(1,1), heights = c(6, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

save_png(grf, "figures/chap4_drift.png", 1280, 1800)


source("header.R")
options(scipen=999)
library(ggpmisc)
library(daltoolbox)
library(harbinger)
library(heimdall)
library(patchwork)


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

model <- dfr_kldist(target_feat = 'serie')
grf_kldist <- event_plot(model, dataset$serie, dataset$event, dataset$serie, "KLD")

model <- dfr_kswin(target_feat = 'serie')
grf_kswin <- event_plot(model, dataset$serie, dataset$event, dataset$serie, "KSWIN")

model <- dfr_page_hinkley(target_feat = 'serie')
grf_page_hinkley <- event_plot(model, dataset$serie, dataset$event, dataset$serie, "Page Hinkley")

model <- dfr_adwin(target_feat='serie')
grf_adwin <- event_plot(model, dataset$serie, dataset$event, dataset$serie, "ADWIN")

grf <- wrap_plots(grf_base, grf_ddm, grf_eddm, grf_hddm, grf_kldist, grf_kswin, grf_page_hinkley, grf_adwin,
                                  ncol = 1,   widths = c(1,1), heights = c(6, 1, 1, 1, 1, 1, 1, 1))

save_png(grf, "figures/chap4_drift.png", 1280, 1584)


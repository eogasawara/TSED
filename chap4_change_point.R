source("header.R")
options(scipen=999)
library(ggpmisc)
library(daltoolbox)
library(harbinger)
library(patchwork)


event_plot <- function(model, serie, event, title) {
  
  model <- fit(model, serie)
  detection <- detect(model, serie)
  
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

model <- hcp_amoc()
grf_amoc <- event_plot(model, dataset$serie, dataset$event, "AMOC")

model <- hcp_binseg(Q = 10)
grf_binseg <- event_plot(model, dataset$serie, dataset$event, "BinSeg")

model <- hcp_pelt()
grf_pelt <- event_plot(model, dataset$serie, dataset$event, "PELT")

model <- hcp_chow()
grf_chow <- event_plot(model, dataset$serie, dataset$event, "Chow test")

model <- hcp_gft()
grf_gft <- event_plot(model, dataset$serie, dataset$event, "GFT")

model <- hcp_scp()
grf_scp <- event_plot(model, dataset$serie, dataset$event, "SCP")

model <- hcp_cf_arima(sw_size = 30)
grf_cf_arima <- event_plot(model, dataset$serie, dataset$event, "CF(ARIMA)")


grf <- wrap_plots(grf_base, grf_amoc, grf_binseg, grf_pelt, grf_chow, grf_gft, grf_scp, grf_cf_arima,
                                  ncol = 1,   widths = c(1,1), heights = c(6, 1, 1, 1, 1, 1, 1, 1))

save_png(grf, "figures/chap4_change_point.png", 1280, 1584)


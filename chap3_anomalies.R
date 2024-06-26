source("header.R")
options(scipen=999)
library(ggpmisc)
library(daltoolbox)
library(harbinger)
library(patchwork)


event_plot <- function(model, serie, event, title) {
  
  model_remd <- fit(model, serie)
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


#loading the example database
data("examples_anomalies")

#Using the tt warped time dataset$serie
dataset <- examples_anomalies$tt_warped
#dataset$event <- factor(dataset$event, labels=c("FALSE", "TRUE"))
head(dataset)

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
grf_base <- grf_base + labs( x = "Time", y = "Example") 

############# Models ################

model <- hanr_remd()
grfremd <- event_plot(model, dataset$serie, dataset$event, "REMD")

model <- hanr_emd()
grfemd <- event_plot(model, dataset$serie, dataset$event, "EMD")

model <- hanr_fbiad()
grffbiad <- event_plot(model, dataset$serie, dataset$event, "FBIAD")

grf <- wrap_plots(grf_base, grfremd, grfemd, grffbiad, 
                  ncol = 1,   widths = c(1,1), heights = c(6, 1, 1, 1))

save_png(grf, "figures/chap3_models.png", 1280, 1280)

jpeg("grafico_Remd_v3.jpeg", width = 15, height = 11, units = "in", res = 300)

print(grf)
dev.off()

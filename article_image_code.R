
############## Import files ################

source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/jupyter.R")
# library(dplyr)

library(hht)#CEEMD
library(ggplot2)
load_library("daltoolbox") 
load_library("harbinger")

library(devtools)
library(dalevents)

############## Create data ################

caminho_arquivo <- "yahoo_sample.RData"
load(caminho_arquivo)

dataset <- yahoo_sample$`A3Benchmark-TS39`
dataset$event <- factor(dataset$event, labels=c("FALSE", "TRUE"))
plot_ts(x = 1:length(dataset$series), y = dataset$series)
series <- dataset$series

############## REMD Model ################

#source("hanr_remd.R")
model_remd <- hanr_remd()
model_remd <- fit(model_remd, series)
detection <- detect(model_remd, series)

grf <- har_plot(model_remd, dataset$series, detection, dataset$event)
plot(grf)

dataset_remd  <- data.frame(col_TP_verde = logical(1680), col_FN_blue = logical(1680), col_FP_red = logical(1680))

dataset_remd$col_TP_verde <- as.logical(dataset$event) & as.logical(detection$event)
dataset_remd$col_FN_blue <- as.logical(dataset$event) & as.logical(!detection$event)
dataset_remd$col_FP_red <- (!as.logical(dataset$event)) & as.logical(detection$event)


############## EMD Model ################

model <- hanr_emd()
model <- fit(model, series)
detection <- detect(model, series)


grf <- har_plot(model, dataset$series, detection, dataset$event)
plot(grf)

dataset_emd  <- data.frame(col_TP_verde = logical(1680), col_FN_blue = logical(1680), col_FP_red = logical(1680))

dataset_emd$col_TP_verde <- as.logical(dataset$event) & as.logical(detection$event)
dataset_emd$col_FN_blue <- as.logical(dataset$event) & as.logical(!detection$event)
dataset_emd$col_FP_red <- (!as.logical(dataset$event)) & as.logical(detection$event)

############## FBIAD Model ################

model <- hanr_fbiad()
model <- fit(model, series)
detection <- detect(model, series)


grf <- har_plot(model, dataset$series, detection, dataset$event)
plot(grf)

dataset_fbiad  <- data.frame(col_TP_verde = logical(1680), col_FN_blue = logical(1680), col_FP_red = logical(1680))

dataset_fbiad$col_TP_verde <- as.logical(dataset$event) & as.logical(detection$event)
dataset_fbiad$col_FN_blue <- as.logical(dataset$event) & as.logical(!detection$event)
dataset_fbiad$col_FP_red <- (!as.logical(dataset$event)) & as.logical(detection$event)

##################### Create image ########################

dataset_remd$x <- 1:1680
dataset_remd$y <- 0

dataset_emd$x <- 1:1680
dataset_emd$y <- 0.25

dataset_fbiad$x <- 1:1680
dataset_fbiad$y <- 0.5

library(patchwork)


######### linha REMD
grf <-  ggplot() + geom_line(data = dataset_remd, aes(x = x, y = y), color = "black") 
grf <- grf + geom_point(data = subset(dataset_remd, col_TP_verde == TRUE), size = 2, col = "green", aes(x = x, y = y))
grf <- grf + geom_point(data = subset(dataset_remd, col_FN_blue == TRUE), size = 2, col = "blue", aes(x = x, y = y))
grf <- grf + geom_point(data = subset(dataset_remd, col_FP_red == TRUE), size = 2, col = "red", aes(x = x, y = y))
grf <- grf + theme_minimal()
grf <- grf + theme(
  panel.background = element_rect(fill = "white"),  # Define a cor de fundo como branco
  panel.grid.major = element_blank(),  # Remove linhas de grade principais
  panel.grid.minor = element_blank(),
  axis.text.y = element_blank() 
)
grf <- grf+ ylab("REMD") + xlab(NULL) 

print(grf)

######### linha FBIAD

grf1 <-  ggplot() + geom_line(data = dataset_fbiad, aes(x = x, y = y), color = "black") 
grf1 <- grf1 + geom_point(data = subset(dataset_fbiad, col_TP_verde == TRUE), size = 2, col = "green", aes(x = x, y = y))
grf1 <- grf1 + geom_point(data = subset(dataset_fbiad, col_FN_blue == TRUE), size = 2, col = "blue", aes(x = x, y = y))
grf1 <- grf1 + geom_point(data = subset(dataset_fbiad, col_FP_red == TRUE), size = 2, alpha= 0.5 , col = "red", aes(x = x, y = y))
grf1 <- grf1 + theme_minimal() 
grf1 <- grf1 + theme(
  panel.background = element_rect(fill = "white"),  # Define a cor de fundo como branco
  panel.grid.major = element_blank(),  # Remove linhas de grade principais
  panel.grid.minor = element_blank(),
  axis.text.y = element_blank() # Remove linhas de grade secundárias
)
grf1 <- grf1+ ylab("FBIAD") + xlab(NULL) 

print(grf1)

######### linha EMD 

grf2 <-  ggplot() + geom_line(data = dataset_emd, aes(x = x, y = y), color = "black") 
grf2 <- grf2 + geom_point(data = subset(dataset_emd, col_TP_verde == TRUE), size = 2, col = "green", aes(x = x, y = y))
grf2 <- grf2 + geom_point(data = subset(dataset_emd, col_FN_blue == TRUE), size = 2,  col = "blue", aes(x = x, y = y))
grf2 <- grf2 + geom_point(data = subset(dataset_emd, col_FP_red == TRUE), size = 2, alpha=0.5, col = "red", aes(x = x, y = y))
grf2 <- grf2 + theme_minimal() 
grf2 <- grf2 + theme(
  panel.background = element_rect(fill = "white"),  # Define a cor de fundo como branco
  panel.grid.major = element_blank(),  # Remove linhas de grade principais
  panel.grid.minor = element_blank(),
  axis.text.y = element_blank(), # Remove linhas de grade secundárias
  
)
grf2 <- grf2+ ylab("EMD") + xlab(NULL) 

print(grf2)


########################## Create full image ######################

dataset$x <- 1:1680

grf_base <- ggplot(data = dataset, aes(x = 1:length(series), y = series)) 
grf_base <- grf_base + geom_line() 
grf_base <- grf_base + geom_point(color = "black", size = 0.5) 
grf_base <- grf_base + geom_point(data = subset(dataset, event == TRUE),  aes(x = x), color = "blue", size = 1.5)   # Adiciona pontos azuis onde dataset$event = TRUE
grf_base <- grf_base + theme_minimal() 
grf_base <- grf_base +theme(
  panel.background = element_rect(fill = "white"), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank()   
)
grf_base <- grf_base + labs( x = "Time", y = "Yahoo Sample: A3Benchmark-TS39") 

print(grf_base)

graficos_combinados <- wrap_plots(grf_base, grf, grf2, grf1, ncol = 1,   widths = c(1,1),
                                  heights = c(10, 1, 1, 1))

print(graficos_combinados)

jpeg("grafico_Remd_v3.jpeg", width = 15, height = 11, units = "in", res = 300)

print(graficos_combinados)
dev.off()

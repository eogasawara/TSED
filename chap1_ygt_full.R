source("header.R")
library(daltoolbox)
library(harbinger)

load("data/noaa-global/temp_yearly.RData")
data <- temp_yearly
data$event <- FALSE

model <- hanr_arima()
model <- fit(model, data$temperature)
detection <- detect(model, data$temperature)
print(detection |> dplyr::filter(event==TRUE))

grf <- har_plot(model, data$temperature, detection, data$event, idx = data$x) +
  font + 
  scale_x_date(breaks = "10 years",  date_labels = "%Y",  limits = c(as.Date("1850-01-01"), as.Date("2030-01-01"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

detectionA <- detection
grfA <- grf

model <- hcp_chow()
model <- fit(model, data$temperature)
detection <- detect(model, data$temperature)
print(detection |> dplyr::filter(event==TRUE))

grf <- har_plot(model, data$temperature, detection, data$event, idx = data$x) +
  font + 
  scale_x_date(breaks = "10 years",  date_labels = "%Y",  limits = c(as.Date("1850-01-01"), as.Date("2030-01-01"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
detectionB <- detection
grfB <- grf


model <- hmo_sax(12, 3, 3)
model <- fit(model, data$temperature)
detection <- detect(model, data$temperature)

print(detection |> dplyr::filter(event==TRUE))

grf <- har_plot(model, data$temperature, detection, data$event, idx = data$x)+
  font + 
  scale_x_date(breaks = "10 years",  date_labels = "%Y",  limits = c(as.Date("1850-01-01"), as.Date("2030-01-01"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
detectionC <- detection
grfC <- grf


detection <- detectionC
detection$event <- detectionA$event | detectionB$event | detectionC$event 
detection$type[detectionA$type != ""] <- detectionA$type[detectionA$type != ""]
detection$type[detectionB$type != ""] <- detectionB$type[detectionB$type != ""]
grf <- har_plot(model, data$temperature, detection, data$event, idx = data$x)+
  font + 
  scale_x_date(breaks = "10 years",  date_labels = "%Y",  limits = c(as.Date("1850-01-01"), as.Date("2030-01-01"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
detectionD <- detection
grfD <- grf
plot(grfD)


mypng(file="figures/chap1_ygt_full.png", width = 1600, height = 1080) 
gridExtra::grid.arrange(grfA, grfB, grfC, grfD,  
                        layout_matrix = matrix(c(1,2,3,4), byrow = TRUE, ncol = 2))
dev.off() 






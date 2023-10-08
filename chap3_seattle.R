source("header.R")
library(readxl)

graphic_seattle <- function() {
  seattle <- read_excel("data/seattle.xlsx")
  seattle$Day <- 1:nrow(seattle)
  seattle$Week <- as.integer(seattle$Day/7) + 1
  seattle$temp <- seattle$Max...4
  #seattle$temp <- as.numeric(seattle$Avg...5)
  seattle$temp <- (seattle$temp - 32)/1.8
  seattle <- seattle |> select(day = Day, week = Week, temp = temp) |> group_by(week) |> summarise(temp = max(temp))
  seattle$event <- FALSE
  seattle$event[12] <- TRUE
  
  #model <- hanr_arima()
  model <- hanr_fbiad(sw_size=12)
  
  # fitting the model
  model <- fit(model, seattle$temp)
  
  # making detections using hanr_fbiad
  detection <- detect(model, seattle$temp)
  print(detection[detection$event,])
  
  
  # ploting the results
  grf <- har_plot(model, seattle$temp, detection, seattle$event)
  grf <- grf + scale_x_continuous(breaks = seq(4, 52, by = 4), "Seattle 2019 (weeks)")
  grf <- grf + ylab("temperature (ºC)")
  grf <- grf + fontstyle + font
  return(grf)
}

graphic_seattle_seq <- function() {
  seattle <- read_excel("data/seattle.xlsx")
  seattle$Day <- 1:nrow(seattle)
  seattle$Week <- as.integer(seattle$Day/7) + 1
  seattle$temp <- seattle$Max...4
  #seattle$temp <- as.numeric(seattle$Avg...5)
  seattle$temp <- (seattle$temp - 32)/1.8
  seattle <- seattle |> select(day = Day, week = Week, temp = temp) |> group_by(week) |> summarise(temp = max(temp))
  seattle$event <- FALSE
  #seattle$event[24:26] <- TRUE
  seattle$temp[12] <- 0.8*seattle$temp[12]
  seattle$temp[19] <- 0.8*seattle$temp[19]
  seattle$temp[26] <- 1.1*seattle$temp[24]
  seattle$temp[25] <- 0.5*seattle$temp[24]
  seattle$temp[35] <- 0.8*seattle$temp[35]
  
  #model <- hanr_arima()
  model <- hanr_fbiad(sw_size=12)
  
  # fitting the model
  model <- fit(model, seattle$temp)
  
  # making detections using hanr_fbiad
  detection <- detect(model, seattle$temp)
  print(detection[detection$event,])

  # ploting the results
  grf <- har_plot(model, seattle$temp, detection, seattle$event)
  grf <- grf + scale_x_continuous(breaks = seq(4, 52, by = 4), "Synthetic time series (weeks)")
  grf <- grf + ylab("temperature (ºC)")
  grf <- grf + fontstyle + font
  return(grf)
}

grfS <- graphic_seattle()
grfSeq <- graphic_seattle_seq()


mypng(file="figures/chap3_seattle.png", width = 1600, height = 720) #144 #720*1.75
gridExtra::grid.arrange(grfS, grfSeq,
                        layout_matrix = matrix(c(1,2), byrow = TRUE, ncol = 2))
dev.off()  


source("header.R")
library(daltoolbox)
library(harbinger)

load("data/noaa-global/temp_monthly.RData")
temp_monthly$year <- as.numeric(format(temp_monthly$x,'%Y'))
data$event <- FALSE

for (i in 1851:2022) { 
  model <- hanr_arima()
  data <- temp_monthly
  data$temperature[data$year > i] <- NA
  model <- fit(model, data$temperature)
  detection <- detect(model, data$temperature)
  detection$year <- i
  detection$temperature <- data$temperature
  
  grf <- har_plot(model, data$temperature, detection, data$event, idx = data$x) +
    font + 
    scale_x_date(breaks = "10 years",  date_labels = "%Y",  limits = c(as.Date("1850-01-01"), as.Date("2030-01-01"))) +
    scale_y_continuous(limits = c(13, 15.5)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  save_png(grf, sprintf("temp/%d.png", i), 1280, 720)
}

save_png(grf, "figures/chap7_offline.png", 1280, 720)

library(gifski) # apt-get install cargo 

png_files <- list.files("temp/", pattern = ".*png$", full.names = TRUE)
output <- "figures/chap7_online.gif"
gifski(png_files, gif_file = output, width = 1280, height = 720, delay = 0.1)


#library(apng)
#apng(c(png_files), output)

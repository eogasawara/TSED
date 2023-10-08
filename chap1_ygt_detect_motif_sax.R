source("header.R")
library(ggpmisc)
library(dplyr)
library(stringr)

#load("data/noaa-global/temp_monthly.RData")
#data <- temp_monthly
load("data/noaa-global/temp_yearly.RData")
data <- temp_yearly
data$event <- FALSE
model <- hmo_sax(16, 3, 3)

model <- fit(model, data$temperature)
event <- rep(FALSE, nrow(data))

detection <- detect(model, data$temperature)

i <- detection$seq == "OOO"
#chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
#chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

detection$event[i] <- FALSE
detection$type[i] <- NA
detection$seq[i] <- NA
detection$seqlen[i] <- NA


print(detection |> dplyr::filter(event==TRUE))

grf <- har_plot(model, data$temperature, detection, event, idx = data$x)+
  fontstyle + font + 
  scale_x_date(breaks = "10 years",  date_labels = "%Y",  limits = c(as.Date("1850-01-01"), as.Date("2030-01-01"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
save_png(grf, "figures/chap1_ygt_detect_motif_sax.png", 1280, 720)

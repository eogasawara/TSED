source("header.R")

#load("data/noaa-global/temp_monthly.RData")
#data <- temp_monthly
load("data/noaa-global/temp_yearly.RData")
data <- temp_yearly
data$event <- FALSE


if (FALSE) {
  model <- hcp_cf_arima(sw_size = 60)
  #model <- hcp_cf_lr(sw_size = 5)
  #model <- hcp_scp(sw_size = 10)
  model <- fit(model, data$temperature)
  detection <- detect(model, data$temperature)
  i <- detection$type == "anomaly"
  detection$event[i] <- FALSE
  detection$type[i] <- ""
  print(detection |> dplyr::filter(event==TRUE))
  print(nrow(detection |> dplyr::filter(event==TRUE)))
  grf <- har_plot(model, data$temperature, detection, data$event, idx = data$x) +
    fontstyle + font + 
    scale_x_date(breaks = "10 years",  date_labels = "%Y",  limits = c(as.Date("1850-01-01"), as.Date("2030-01-01"))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  save_png(grf, "figures/chap1_mgt_detect_cf.png", 1280, 720)
}

library(strucchange)
library(ggplot2)

# Generate synthetic time series data with a change point
y <- data$temperature
x <- 1:length(y)

# Perform using f-test
model <- Fstats(y ~ x)

breaks <- breakpoints(model)

model <- harbinger()
#model <- hcp_cf_lr(sw_size = 5)
#model <- hcp_scp(sw_size = 10)
model <- fit(model, data$temperature)
detection <- detect(model, data$temperature)
i <- breaks$breakpoints
detection$event[i] <- FALSE
detection$type[i] <- "changepoint"
print(detection |> dplyr::filter(event==TRUE))
print(nrow(detection |> dplyr::filter(event==TRUE)))
grf <- har_plot(model, data$temperature, detection, data$event, idx = data$x) +
  fontstyle + font + 
  scale_x_date(breaks = "10 years",  date_labels = "%Y",  limits = c(as.Date("1850-01-01"), as.Date("2030-01-01"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
save_png(grf, "figures/chap1_ygt_detect_cf.png", 1280, 720)


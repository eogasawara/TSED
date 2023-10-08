source("header.R")

#load("data/noaa-global/temp_monthly.RData")
#data <- temp_monthly
load("data/noaa-global/temp_yearly.RData")
data <- temp_yearly
data$event <- FALSE

custom_har_outliers <- function(data){
  org = length(data)
  cond <- rep(FALSE, org)
  q = stats::quantile(data, na.rm=TRUE)
  IQR = q[4] - q[2]
  lq1 = as.double(q[2] - 3*IQR)
  hq3 = as.double(q[4] + 3*IQR)
  cond = data > hq3
  return (cond)
}

custom_har_outliers_idx <- function(data){
  cond <- custom_har_outliers(data)
  index.cp = which(cond)
  return (index.cp)
}


model <- hanr_arima()
#model <- hanr_fbiad(sw_size = 120)
#model$har_outliers <- custom_har_outliers
#model$har_outliers_idx <- custom_har_outliers_idx
model <- fit(model, data$temperature)
detection <- detect(model, data$temperature)
print(detection |> dplyr::filter(event==TRUE))
print(nrow(detection |> dplyr::filter(event==TRUE)))
grf <- har_plot(model, data$temperature, detection, data$event, idx = data$x) +
  fontstyle + font + 
  scale_x_date(breaks = "10 years",  date_labels = "%Y",  limits = c(as.Date("1850-01-01"), as.Date("2030-01-01"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
save_png(grf, "figures/chap1_ygt_detect_arima.png", 1280, 720)


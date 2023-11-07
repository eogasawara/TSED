source("header.R")
library(dplyr)
load("data/noaa-global/temp_monthly.RData")
data <- temp_monthly
data$event <- FALSE

library(forecast)

data <- data |> dplyr::filter(x > as.Date("1970-01-01"))

ts_data <- ts(data$temperature, frequency = 12, start = c(1970, 1))

# Perform a seasonal decomposition to separate the trend, seasonal, and random components of the time series
decomp <- decompose(ts_data)
#decomp <- decompose(ts_data, type="multiplicative")

# Plot the decomposed components to inspect the seasonality
grf <- autoplot(decomp, labels = c("trend", "seasonal", "residual"))
grf <- grf + theme_bw(base_size = 10) + geom_point(size = 0.25)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
grf <- grf  + font
save_png(grf, "figures/chap2_mgt_decomposition.png", 1280, 1080) #720*2

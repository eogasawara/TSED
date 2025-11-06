library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(forecast)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

# Chapter 2: Mgt Decomposition
# Overview:
# - Loads example dataset (if applicable)
# - Fits a detector and runs event detection
# - Plots results with events overlaid
#



# Load example dataset

data(examples_harbinger)

data <- examples_harbinger$global_temperature_monthly
data$event <- FALSE



data <- data |> dplyr::filter(i > as.Date("1970-01-01"))
ts_data <- ts(data$serie, frequency = 12, start = c(1970, 1))
# Seasonal-trend decomposition (additive)
decomp <- decompose(ts_data)
# Alternative: multiplicative decomposition
# decomp <- decompose(ts_data, type = "multiplicative")

grf <- autoplot(decomp, labels = c("trend", "seasonal", "residual"))

grf <- grf + theme_bw(base_size = 10) + geom_point(size = 0.25)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
grf <- grf  + font

#save_png(grf, "figures/chap2_mgt_decomposition.png", 1280, 1080) # 720*2
grf

library(ggplot2)
library(RColorBrewer)
library(ggpmisc)
library(daltoolbox)
library(harbinger)
library(patchwork)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)




event_plot <- function(model, serie, event, title) {
  # Fit model and detect
  model <- fit(model, serie)
  detection <- detect(model, serie)
  # Remove anomalies for this view
  detection$event[detection$type == "anomaly"] <- FALSE
  detection$type[detection$type == "anomaly"]  <- ""
  # Build compact timeline dataframe
  df <- data.frame(col_TP = as.logical(event) & as.logical(detection$event),
                   col_FN = as.logical(event) & as.logical(!detection$event),
                   col_FP = (!as.logical(event)) & as.logical(detection$event))
  df$x <- seq_along(serie)
  df$y <- 0
  grf <- ggplot() + geom_line(data = df, aes(x = x, y = y), color = "black")
  grf <- grf + geom_point(data = subset(df, col_TP == TRUE), size = 2, col = "green", aes(x = x, y = y))
  grf <- grf + geom_point(data = subset(df, col_FN == TRUE), size = 2, col = "blue",  aes(x = x, y = y))
  grf <- grf + geom_point(data = subset(df, col_FP == TRUE), size = 2, col = "red",   aes(x = x, y = y))
  grf <- grf + theme_minimal()
  grf <- grf + theme(
    panel.background = element_rect(fill = "white"),  # white background
    panel.grid.major = element_blank(),                # no major grid
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank()
  )
  grf <- grf + ylab(title) + xlab(NULL)
  return(grf)
}

# Load example dataset
data(examples_changepoints)

dataset <- examples_changepoints$complex
#### Time Series
dataset$x <- 1:length(dataset$serie)

grf_base <- ggplot(data = dataset, aes(x = seq_along(serie), y = serie))
grf_base <- grf_base + geom_line()
grf_base <- grf_base + geom_point(color = "black", size = 0.5)
grf_base <- grf_base + geom_point(data = subset(dataset, event == TRUE), aes(x = x), color = "blue", size = 1.5)   # add blue points where event = TRUE
grf_base <- grf_base + theme_minimal()
grf_base <- grf_base + theme(
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)
grf_base <- grf_base + labs(x = "time", y = "values")

model <- hcp_amoc()

grf_amoc <- event_plot(model, dataset$serie, dataset$event, "AMOC")

model <- hcp_binseg(Q = 10)

grf_binseg <- event_plot(model, dataset$serie, dataset$event, "BinSeg")

model <- hcp_pelt()

grf_pelt <- event_plot(model, dataset$serie, dataset$event, "PELT")

model <- hcp_chow()

grf_chow <- event_plot(model, dataset$serie, dataset$event, "Chow test")

model <- hcp_gft()

grf_gft <- event_plot(model, dataset$serie, dataset$event, "GFT")

model <- hcp_scp(sw_size = 60)

grf_scp <- event_plot(model, dataset$serie, dataset$event, "SCP")



model <- hcp_cf_ets(sw_size = 60)

grf_cf_arima <- event_plot(model, dataset$serie, dataset$event, "CF(ETS)")
grf <- wrap_plots(grf_base, grf_amoc, grf_binseg, grf_pelt, grf_chow, grf_gft, grf_scp, grf_cf_arima, ncol = 1, widths = c(1, 1), heights = c(6, 1, 1, 1, 1, 1, 1, 1))
#save_png(grf, "figures/chap4_change_point.png", 1280, 1584)
grf

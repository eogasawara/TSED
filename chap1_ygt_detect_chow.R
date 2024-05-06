source("header.R")
library(daltoolbox)
library(harbinger)

data(examples_harbinger)
data <- examples_harbinger$global_temperature_yearly
data$event <- FALSE


model <- hcp_chow()
model <- fit(model, data$serie)
detection <- detect(model, data$serie)
print(detection |> dplyr::filter(event==TRUE))

grf <- har_plot(model, data$serie, detection, data$event, idx = data$i) +
  font + 
  scale_x_date(breaks = "10 years",  date_labels = "%Y",  limits = c(as.Date("1850-01-01"), as.Date("2030-01-01"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
save_png(grf, "figures/chap1_ygt_detect_chow.png", 1280, 720)


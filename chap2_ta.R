source("header.R")

load("data/noaa-global/temp_monthly.RData")
data <- temp_monthly
data$event <- FALSE

model <- harbinger()
model <- fit(model, data$temperature)
detection <- detect(model, data$temperature)
grf <- har_plot(model, data$temperature, detection, data$event, idx = data$x, pointsize=0.25) +
  fontstyle + font + 
  scale_x_date(breaks = "10 years",  date_labels = "%Y",  limits = c(as.Date("1850-01-01"), as.Date("2030-01-01"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), plot.caption=element_text(hjust = 0.5)) 
grf <- grf + ylab("temperature")
grf <- grf + xlab("monthly")
grf <- grf + labs(caption = "(a)") 
grfM <- grf

load("data/noaa-global/temp_yearly.RData")
data <- temp_yearly
data$event <- FALSE

model <- harbinger()
model <- fit(model, data$temperature)
detection <- detect(model, data$temperature)
grf <- har_plot(model, data$temperature, detection, data$event, idx = data$x) +
  fontstyle + font + 
  scale_x_date(breaks = "10 years",  date_labels = "%Y",  limits = c(as.Date("1850-01-01"), as.Date("2030-01-01"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), plot.caption=element_text(hjust = 0.5)) 
grf <- grf + ylab("temperature")
grf <- grf + xlab("yearly")
grf <- grf + labs(caption = "(b)") 
grfY <- grf

mypng(file="figures/chap2_ta.png", width = 1280, height = 1080) #144 #720*1.5
gridExtra::grid.arrange(grfM, grfY, layout_matrix = matrix(c(1,2), byrow = TRUE, ncol = 1))
dev.off() 



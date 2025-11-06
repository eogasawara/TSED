library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")



n <- 78  # number of time points
data <- c(sin((0:n) / pi), 2 * sin((0:19) / pi), sin((0:n) / pi))
event <- rep(FALSE, n)

model <- hanr_garch()

model <- fit(model, data)

detection <- detect(model, data)

print(detection |> dplyr::filter(event == TRUE))
print(nrow(detection |> dplyr::filter(event == TRUE)))

grf <- har_plot(model, data, detection) + font +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

grf <- grf + geom_vline(xintercept = 79, col = "darkgray", linewidth = 0.5, linetype = "dashed")
grf <- grf + geom_vline(xintercept = 99, col = "darkgray", linewidth = 0.5, linetype = "dashed")

#save_png(grf, "figures/chap3_garch.png", 1280, 720)
grf

source("header.R")
library(daltoolbox)
library(harbinger)

load("data/noaa-global/temp_yearly.RData")

ts_data <- ts(temp_yearly$temperature, frequency=1, start = c(1850, 1))
idx <- time(ts_data)

model <- fit(hmo_mp(w = 6, qtd = 3), ts_data)
detection <- detect(model, ts_data)
detection$event <- detection$seq == 1
detection$type[detection$seq != 1] <- NA
detection$seq[detection$seq != 1] <- NA
detection$seqlen[detection$seq != 1] <- NA

grf <- har_plot(model, ts_data, detection, idx=idx)
grf <- grf + font

save_png(grf, "figures/chap5_motifs_mp.png", 1280, 720)


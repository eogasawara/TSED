source("header.R")
library(daltoolbox)
library(harbinger)

data(examples_harbinger)

temp_yearly <- examples_harbinger$global_temperature_yearly

ts_data <- ts(temp_yearly$serie, frequency=1, start = c(1850, 1))
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

